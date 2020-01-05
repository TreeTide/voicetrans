# VoiceTrans

VoiceTrans is a (mostly) hands-free translator for language learners.
The idea is to let the user read a (physical) book or article uninterrupted,
asking for unknown words in a natural voice.

## Architecture

Components:
- Recognition server
- Web frontend.

The frontend uses the HTML5 Media Capture API to get a 44kHz mic stream, and
send it chunked to the server.

The server resamples on-the-fly to 16kHz, the maximum accepted by/meaningful for
voice recognition backend, and streams to the recognition backend. Once the
client stream is closed and the recoginition result is available, translates
the recognized text, and returns the text pair to the frontend for display.

## Building

Prequisites:
- Install the Stack tool from http://www.haskellstack.org.
- Install and run the Docker daemon. See
  [Installing](http://docs.haskellstack.org/en/stable/docker_integration/#prerequisites)
  and [Security](http://docs.haskellstack.org/en/stable/docker_integration/#security).

Components:
- voicetrans-server: do `stack image container` in that dir.
- voicetrans-frontend: do `stack build -t treetide/voicetrans-frontend .` in
  that dir.

