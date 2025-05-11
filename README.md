# oscl

<p align="center">
  <img src="https://github.com/user-attachments/assets/05bb8f4d-599d-4212-a7f4-337f7aed17db" />
</p>

## Overview

Minimal OSC toolkit written in Common Lisp.  
`oscl` provides simple CLI-based OSC message sending and receiving functionality.  
It is designed for use in scripting, testing, and lightweight toolchains where GUI-based tools are overkill.

## Usage Examples

### send command

```bash
./oscl.ros send --host 127.0.0.1 --port 9000 --address "/test" --args "1 2.0 hello" --interval 1000
```

- `--host`, `--port`, and `--address` are required.
- `--args` and `--interval` are optional.
- `--interval` is in milliseconds. Press `Ctrl+C` to stop repeated sending.

### recv command

```bash
./oscl.ros recv --port 9000
```

- `--port` is optional. Default is `9000`.
- Press `Ctrl+C` to exit cleanly.

## Install

### Build from source

You will need [Roswell](https://github.com/roswell/roswell) and [SBCL](http://www.sbcl.org/) installed.

```
git clone https://github.com/ogrew/oscl.git
cd oscl
ros build oscl.ros
sudo mv oscl /usr/local/bin/
sudo chmod +x /usr/local/bin/oscl
```

Then:

```
oscl recv --port 9000
oscl send --host 127.0.0.1 --port 9000 --address "/light/on"
```

## TODO

- [ ] Support for `#bundle` OSC message type in `recv`
- [ ] JSON output mode in `recv`

## Notes

- This tool uses SBCL-specific system calls such as `sb-sys:enable-interrupt`.  
  Therefore, it may not work correctly with other Lisp implementations.

## License

MIT License
