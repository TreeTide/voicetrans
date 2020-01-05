- pocketsphinx
  - to get /dev/dsp with also, do 'modprobe snd_pcm_oss' which adds emulation device
  - building
    - nix-shell -p haskellPackages.c2hs pocketsphinx sphinxbase ghc
       - HACK: replace the include dirs in the cabal file to point to pocketsphinx and sphinxbase
       - cabal new-build

