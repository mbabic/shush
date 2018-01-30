# shush

Audio playback in Haskell using OpenAL (y not) for to make learnings. Eventually: noise generation, mixing/composition, other fun.

### todo

- noise generation and playback
- add support for more file formats
- eliminate array copying as much as possible when playing files
- basic functionality:
  - play file vs. generate noise (via throwaway cmd line interface?)
  - allow for basic mixing and combination of files to create new sounds
    - is this a ui? a conf file that's parsed (ew) for now?

### Development Notes

#### OpenAL

- look at [Foreign.Marshal.Array](http://hackage.haskell.org/package/base-4.10.1.0/docs/Foreign-Marshal-Array.html) 
  for some nicer ways pointers to arrays
  - less efficient but probably nice code in the end, snoop around to see if 
    something like this exists for ByteStrings
- a nice little basic walkthrough of the core primitives in C: https://ffainelli.github.io/openal-example/

#### .WAV Parsing
##### Format Specs

Source: http://truelogic.org/wordpress/2015/09/04/parsing-a-wav-file-in-c/

*Note: all multi-byte numeric values are little-endian*

| Byte Positions | Value / Sample Value | Descriptions |
| ---- | --- | --- |
| 1 - 4   | "RIFF" | Marks the file as a riff file. ASCII chars. |
| 5 - 8   |        | Total file size including header.  32bit integer. |
| 9 - 12  | "WAVE" | File type header. For .wav this is always "WAVE". |
| 13 - 16 | "fmt " | Format chunk marker. Includes trailing null (so 4 bytes). |
| 17 - 20 | 16     | Length of format data in bytes. 32 bit integer. (?) |
| 21 - 22 | 1      | Type of format. 1 is PCM. 2 byte integer. |
| 23 - 24 | 2      | Number of channels. 2 bit integer. |
| 25 - 28 | 44100  | Sample rate (sample/sec). 32 bit integer. 44100 == CD, 48000 == DAT |
| 29 - 32 | 176400 | (Sample Rate * Bits/Sample) / 8. 32 bit integer. |
| 33 - 34 | 4      | (Bits/Sample * Channels) / 8 |
| 35 - 36 | 16     | Bits/Sample |
| 37 - 40 | "data" | "data" chunk header which marks the begeinning of the data section. |
| 41 - 44 |        | The size of the data section. 32 bit integer. |

##### Examples
- there is a [sample .wav parser in Haskell](https://hackage.haskell.org/package/HCodecs-0.5/docs/src/Codec-Wav.html#parseWav)
  defined as part of the HCodecs package
  - the parser defines its owner Parser monad and combinators to work on 
    bytestrings
- there's [a random bytestring parser](https://hackage.haskell.org/package/bsparse-0.0.5/docs/src/Data-ByteString-Parse.html#Parser) that adopts a similar strategy to the HCodecs one
- [attoparsec has its own bytestring parser](https://hackage.haskell.org/package/attoparsec-0.13.2.2/docs/Data-Attoparsec-ByteString.html)
  that seems to define the primitives you need in order to write combinators to
  parse a .wav file

##### sample.wav
| Attribute | Value |
| --- | --- |
| Download URL | https://freewavesamples.com/roland-jv-2080-pick-bass-c2 |
| Sample Rate  | 44100 Hz |
| Format       | Stereo |
| Bit Depth    | 16 Bit |

##### sample2.wav
| Attribute | Value |
| --- | --- |
| Download URL | https://freewavesamples.com/casio-mt-45-16-beat |
| Sample Rate  | 44100 Hz |
| Format       | Stereo |
| Bit Depth    | 16 Bit |

#### Noise Generation

##### Example

- Small example. Generated a 1000 byte sign wave and created buffer data as 
  follows:

```
alBufferData(buffer, AL_FORMAT_MONO8, test, 1000, 11024)
```

- There is a [Noise](https://hackage.haskell.org/package/Noise) package on 
  Hackage. Worth at least reading through and understanding what's happening,
  specifically implementation wrt [Perlin noise](https://en.wikipedia.org/wiki/Perlin_noise)
  might be illuminating even if it's not intended for noise generation
  
- [DSP Generation of Pink Noise](http://www.firstpr.com.au/dsp/pink-noise/)

- Is there a way to stream generated data to OpenAL? Better than creating a big 
  buffer of noise and looping it.
