# oscl

<p align="center">
  <img src="https://github.com/user-attachments/assets/7483f06c-d49a-4041-9fab-2e84f928bca1" />
</p>

## Overview

`oscl` is a minimal command-line toolkit for working with [Open Sound Control (OSC)](https://opensoundcontrol.stanford.edu), written in [Common Lisp](https://common-lisp.net) for both educational and practical use.  
It allows for easy sending and receiving of OSC messages from the terminal, making it useful for scripting, testing, and quick integration with OSC-enabled systems.

Originally created as a Lisp learning project, `oscl` has grown into a practical tool for controlling and monitoring OSC-based workflows — especially in creative coding, live performance, and interactive installations.

## Usage Examples

### send command

```bash
> oscl send \
  --host 127.0.0.1 \
  --port 9000 \
  --address "/test" \
  --args "1 2.0 hello" \
  --interval 1000
```

- `--host`, `--port`, and `--address` are required.
- `--args` and `--interval` are optional.
- `--interval` is in milliseconds. Press `Ctrl+C` to stop repeated sending.
- `--host localhost` will be automatically converted to `--host 127.0.0.1`.

### recv command

```bash
> oscl recv --port 7000 --filter "point"
```

- `--port` is optional. Default is `9000`.
- Use `--filter <string>` to show only messages whose address includes the string.
  - Prefix with `-` to exclude matching addresses: e.g. `--filter -test`
- Use `--raw` to display the first 64 bytes of raw data in hexadecimal.
- Press `Ctrl+C` to exit cleanly.

## Install

### Build from source

You will need [Roswell](https://github.com/roswell/roswell) and [SBCL](http://www.sbcl.org/) installed.

```bash
> git clone https://github.com/ogrew/oscl.git
> cd oscl
> ros build oscl.ros
> sudo mv oscl /usr/local/bin/
> sudo chmod +x /usr/local/bin/oscl
```

Then:

```bash
> oscl recv --port 9000
> oscl send --host 127.0.0.1 --port 9000 --address "/light/on"
```

## TODO

## TODO

- [ ] Support for `#bundle` OSC message type in `recv`
- [ ] JSON output mode in `recv`
- [ ] `record` mode: log incoming OSC messages to a file (e.g. as JSON or plain text)
- [ ] `play` mode: replay recorded OSC messages to a target host/port with timing preserved
- [ ] `bridge` mode: forward OSC messages from one address/port to another, with optional filtering

## Notes

- This tool uses SBCL-specific system calls such as `sb-sys:enable-interrupt`.  
  Therefore, it may not work correctly with other Lisp implementations.

## License

[MIT License](https://github.com/ogrew/oscl/blob/main/LICENSE)
