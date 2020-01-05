window.onload = function () {
  navigator.getUserMedia =
      navigator.getUserMedia || navigator.webkitGetUserMedia;
  window.AudioContext =
      window.AudioContext || window.webkitAudioContext;
  if (!navigator.getUserMedia || !window.AudioContext) {
    alert('Some dependent API is not supported :/');
  }

  function debugEvents(name, node) {
    function debug(e) {
      console.log(name + ': ' + e);
    }
    function add(eventName) {
      node.addEventListener(eventName, debug);
    }
    add('statechange');
    add('complete');
    add('ended');
    add('message');
    add('loaded');
    add('nodecreate');
  }

  function errorCb(e) {
    alert('Error getting audio: ' + e);
  }

  var resultDiv = document.getElementById('result');
  function sendData(pcm) {
    var req = new XMLHttpRequest();
    req.open('POST', '/api/translate', true);
    req.onload = function(e) {
      resultDiv.innerHTML = req.responseText;
    };
    // TODO(robinp): error handling
    resultDiv.innerHTML = '...fetching...';
    req.send(pcm);
  }
  function sendChunk(pcm) {
    var req = new XMLHttpRequest();
    req.open('POST', '/api/chunk', true);
    req.onload = function(e) {
      resultDiv.innerHTML = '...' + req.responseText + '...';
    };
    // TODO(robinp): error handling
    resultDiv.innerHTML = '...streaming...';
    req.send(pcm);
  }
  function stopChunk() {
    var req = new XMLHttpRequest();
    req.open('POST', '/api/endchunk', true);
    req.onload = function(e) {
      resultDiv.innerHTML = req.responseText;
    };
    // TODO(robinp): error handling
    resultDiv.innerHTML = '...stopping...';
    req.send();
  }

  function successCb(stream) {
    var audioCtx = new AudioContext();
    var mic = audioCtx.createMediaStreamSource(stream);
    console.log('Called with: ' + stream
        + ', channel count: ' + mic.channelCount
        + ', channel cnt mode: ' + mic.channelCountMode
        + ', channel interp: ' + mic.channelInterpretation);
    //var mic = audioCtx.createBufferSource();
    //mic.buffer = stream;
    var channels = mic.channelCount;
    var proc = audioCtx.createScriptProcessor(0, channels, channels);
    var state = {
      ci: 0,
      collected: new Float32Array(44100),
      rem: 44100*5,
      done: false,
      first: true,
      recording: false,
    };
    var btn = document.getElementById('micBtn');
    btn.addEventListener('click', function() {
      if (state.recording) {
        console.log('Yet recording.');
      } else {
        state.collected = new Float32Array(44100);
        state.ci = 0;
        state.rem = 44100*5;
        state.recording = true;
        btn.innerHTML = 'Recording.';
        resultDiv.innerHTML = '...rec...';
      }
    });

    window.keepTheseFromGC = [audioCtx, mic, proc];

    var eventCount = 0;
    proc.addEventListener('audioprocess', function(e) {
      var collected = state.collected;
      var dbg = document.getElementById('counter');
      dbg.innerHTML = '' + eventCount++ + ' oCI ' + state.ci
          + ' recording ' + state.recording + ' CL ' + collected.length;
      var buf = e.inputBuffer;
      var originalCollectionIndex = state.ci;
      for (var c = 0; c < 1 /*channels*/; ++c) {
        var data = buf.getChannelData(c);
        state.ci = originalCollectionIndex;
        if (state.recording) {
          var remain = Math.max(0, Math.min(state.rem, data.length));
          dbg.innerHTML += ' - remain ' + remain + ' ci ' + state.ci;
          if (remain == 0) {
            console.log('Done recording.');
            state.recording = false;
            btn.innerHTML = 'Done.';
            //sendData(collected);
            stopChunk()
          } else {
            var di = 0;
            while (di < data.length && state.rem > 0) {
              var freespace = collected.length - state.ci;
              var fill = Math.min(freespace, data.length - di);
              for (var i = 0; i < fill; ++i) {
                collected[state.ci++] = data[di++]; // += data[di++]; // / channels;
              }
              state.rem -= fill;
              if (freespace == fill) {
                sendChunk(collected);
                state.ci = 0;
              }
            }
          }
        }
        // Commented out, since no need for feedback.
        // e.outputBuffer.copyToChannel(data, c);
        if (state.first) {
          console.log('proc input channels: ' + buf.numberOfChannels);
          console.log('proc output channels: ' + e.outputBuffer.numberOfChannels);
          console.log('sampleRate: ' + buf.sampleRate);
          console.log('frames: ' + buf.length)
          state.first = false;
        }
      }
    });
    //var analyser = audioCtx.createAnalyser();
    //mic.connect(analyser);
    //debugEvents('mic', mic);
    //debugEvents('proc', proc);
    mic.connect(proc); //audioCtx.destination); //proc);
    proc.connect(audioCtx.destination);
    console.log('Connected things.');
    //mic.start(0);
  }
  // TODO: let choosing audio source.
  console.log('calling gum.');
  navigator.getUserMedia({
    audio: {
      // See https://bugs.chromium.org/p/chromium/issues/detail?id=420625,
      // without this recognition is better.
      mandatory: {
          googEchoCancellation: false,
          googAutoGainControl: false,
          googNoiseSuppression: false,
          googHighpassFilter: false
      },
      optional: []
    },
  }, successCb, errorCb);
  /*
  var h = new XMLHttpRequest();
  h.open('GET', 'http://rigo:3000/wave.wav', true);
  h.responseType = 'arraybuffer';
  h.onload = function() {
    audioCtx.decodeAudioData(h.response, function(buffer) {
      successCb(buffer);
    }, errorCb);
  };
  h.send();
  */
};

/* TODO

getUserMedia(
    	{
            "audio": {
                "mandatory": {
                    "googEchoCancellation": "false",
                    "googAutoGainControl": "false",
                    "googNoiseSuppression": "false",
                    "googHighpassFilter": "false"
                },
                "optional": []
            },
        }, gotStream);

*/
