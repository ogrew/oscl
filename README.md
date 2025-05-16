# oscl

<p align="center">
  <img src="https://github.com/user-attachments/assets/7483f06c-d49a-4041-9fab-2e84f928bca1" />
</p>

## Overview

`oscl` is a minimal command-line toolkit for working with [Open Sound Control (OSC)](https://opensoundcontrol.stanford.edu), written in [Common Lisp](https://common-lisp.net) for both educational and practical use.  
It allows for easy sending and receiving of OSC messages from the terminal, making it useful for scripting, testing, and quick integration with OSC-enabled systems.

Originally created as a Lisp learning project, `oscl` has grown into a practical tool for controlling and monitoring OSC-based workflows — especially in creative coding, live performance, and interactive installations.

## Usage Examples

### `send` command

```bash
> oscl send \
  --host 127.0.0.1 \
  --port 7000 \
  --address "/test" \
  --args "1 2.0 hello" \
  --interval 1000
```

- `--host`, `--port`, and `--address` are required.
- `--args` and `--interval` are optional.
- `--interval` is in milliseconds. Press `Ctrl+C` to stop repeated sending.
- `--host localhost` will be automatically converted to `--host 127.0.0.1`.

You can also send messages from a JSON file:

```bash
> oscl send \
  --host localhost
  --port 9010
  --from data.json
  --interval 1000
```

- `--from` takes a path to a JSON file describing a list of OSC messages.
- `--interval` controls the delay (in ms) between messages.
- When using `--from`, `--address` and `--args` must not be used together.

### `recv` command

```bash
> oscl recv \
  --port 7000 \
  --filter "point"
```

- `--port` is optional. Default is `9000`.
- Use `--filter <string>` to show only messages whose address includes the string.
  - Prefix with `-` to exclude matching addresses: e.g. `--filter -test`
- Use `--raw` to display the first 64 bytes of raw data in hexadecimal.
- Press `Ctrl+C` to exit cleanly.

### `bridge` command

```bash
> oscl bridge \
  --in-host localhost \
  --in-port 7001 \
  --out-host 127.0.0.4 \
  --out-port 7010 \
  --filter "light"
```

- Receives OSC messages on `--in-host / --in-port` and forwards them to `--out-host / --out-port`.
- `--filter <string>` lets you forward only messages whose address includes the string.
- Use `--filter -<string>` to exclude matching messages.
- `--in-host` localhost is automatically converted to `127.0.0.1`.
- Press `Ctrl+C` to stop bridging.

## Install

The easiest way to install `oscl` is now via Homebrew! 🍻

> **Note:** oscl currently supports **Apple Silicon (arm64) Macs** only.

### Install with Homebrew (Recommended)

Step 1. Tap the repository:

```bash
> brew tap ogrew/oscl
```

Step 2. Install oscl:

```bash
> brew install oscl
```

Step 3. Verify installation:

```bash
> oscl --help
```

That's it! Now you can use `oscl` from anywhere.

### Build from source (Alternative)

If you prefer building manually or are not using Homebrew, you can build visp from source.

You will need [Roswell](https://github.com/roswell/roswell) and [SBCL](http://www.sbcl.org/) installed.

```bash
> git clone https://github.com/ogrew/oscl.git
> cd oscl
> ros build oscl.ros
```

Move the generated binary into a directory included in your `$PATH` (e.g., `/usr/local/bin`):

```bash
> sudo mv oscl /usr/local/bin/
> sudo chmod +x /usr/local/bin/oscl
```

Then you can use it like this:

```bash
> oscl --help
```

## TODO

- [ ] Support for `#bundle` OSC message type in `recv`
- [ ] JSON output mode in `recv`
- [ ] `record` mode: log incoming OSC messages to a file (e.g. as JSON or plain text)
- [ ] `play` mode: replay recorded OSC messages to a target host/port with timing preserved
- [x] `bridge` mode: forward OSC messages from one address/port to another, with optional filtering

## Notes

- This tool uses SBCL-specific system calls such as `sb-sys:enable-interrupt`.  
  Therefore, it may not work correctly with other Lisp implementations.

## License

[MIT License](https://github.com/ogrew/oscl/blob/main/LICENSE)
