[![Unit Tests](https://github.com/Backseating-Committee-2k/Seatbelt/actions/workflows/cmake.yml/badge.svg)](https://github.com/Backseating-Committee-2k/Seatbelt/actions/workflows/cmake.yml)
[![Licence: MIT](https://img.shields.io/github/license/Backseating-Committee-2k/Seatbelt)](https://github.com/Backseating-Committee-2k/Seatbelt/blob/main/LICENSE)
[![Discord Channel](https://img.shields.io/discord/834834066008309800?style=social)](https://discord.gg/WygnW2wZj3)
[![Twitch live status](https://img.shields.io/twitch/status/coder2k?style=social)](https://twitch.tv/coder2k)

# Seatbelt

This is the official compiler for the Backseat language (not to be confused with [Backlang](https://www.backlang.org)).
The Backseat language is the official language to program software for
the [Backseat-Safe System 2k](https://github.com/Backseating-Committee-2k/BackseatSafeSystem2k).

## Features

- compiles Backseat code into Bssembler code which can be bssembled with
  the [Upholsterer2k](https://github.com/Backseating-Committee-2k/Upholsterer2k/)
- imperative
- procedural

## How to build

The project uses [CMake](https://cmake.org/) as build system. All dependencies are fetched automatically. Build the
software like so:

```
git clone https://github.com/Backseating-Committee-2k/Seatbelt.git
cd Seatbelt
cmake -B build
make -C build
./build/Seatbelt --output output_filename.bsm my_input_file.bs
```

## FAQ

#### What is the goal of this project?

The project's main goal is to learn how to write a compiler. Another goal is to provide a language that targets
the [Backseat-Safe System 2k](https://github.com/Backseating-Committee-2k/BackseatSafeSystem2k).

#### What's the design philosophy behind the Backseat language?

The language is designed in the way that I like it to be.
