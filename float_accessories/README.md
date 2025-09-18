# Float Accessories - VESC Express Package
Float Accessories is a VESC Express Package for self-balancing skateboards. It aims to:
- Provide a polished and full-featured user experience
- Maintain a clean and reliable codebase that is easy to extend
- Provide code for a lighting module.

_**If you're looking for README of the actual package, you can find it [here](package_README.md).**_

## Contributing
Contributions are welcome and appreciated, please refer to [Contributing](CONTRIBUTING.md).

## Building
### Requirements
- `gcc-arm-embedded` version 13 or higher
- `make`
- `vesc_tool`

To build the package, run:
```sh
make
```

Note a new beta (as of writing this, unreleased) version of `vesc_tool` is needed for the above to work. To build the package with the current / old `vesc_tool` version, use:
```sh
make OLDVT=1
```

If you don't have `vesc_tool` in your `$PATH` (but you have, for example, a downloaded `vesc_tool` binary), you can specify the `vesc_tool` to use:
```sh
make VESC_TOOL=/path/to/vesc_tool
```

## Documentation
[Development Documentation](doc/index.md)
