# Contributing
Bugfix Pull Requests are always welcome. If you want to implement a feature, it is advised get in touch first, to make sure your feature is in line with the direction Float Accessories development is heading.

## Commit Messages
Use descriptive (but succint) commit title. For non-trivial commits, add a short commit description with any due explanation.

If the commit fixes a bug or adds a feature that should be mentioned in the changelog, add a `Fix:` or `Feature:` [git trailer](https://alchemists.io/articles/git_trailers) respectively.

Note the trailers need to go at the end of the commit description and be preceded by an empty line.

A limitation of trailers is they don't support formatted multiline content. A workaround is in place for this, use this format for multiline changelog records:

```
commit a1b2c3d4e5f67890abcdef1234567890abcdef12
Author: Syler Clayton <syler.clayton@gmail.com>
Date:   Sun Sep 3 12:53:68 2025 -0700

    Add support for trans pride colors

    Feature: Add support for trans pride color pattern.
     >
     Example of multiline
     >
     Example of multiline
     
```

That is, use `>` as a new line marker and indent all lines with a single space.

The `changelog.py` script is used to generate the final changelog (it's not perfect, sometimes minor hand-tuning is needed), you can try to run it to see if your trailer works properly. It processes commits in history until it encounters the first version tag (excluding the HEAD commit even if it is tagged).

## Pull Requests (PRs)
- Each commit should be a single independent change, be functional by itself and not contain unrelated changes.
- Use _rebase workflow_ for all PRs.
- When addressing review comments, if possible, update original commits with fixes belonging to them. Then force-push.

## CI
CI runs on every push to main and needs to pass on every PR. It:
- Runs a build of the package.
- Checks formatting of C sources using `clang-format`.

## Code Formatting
C code is automatically formatted using `clang-format` version 18 (you need to provide this dependency yourself).

The rest of the code (namely, the QML AppUI sources) is not auto-formatted. Please do your best to adhere to formatting you see surrounding the code you're editing.

You can format C source files via `clang-format` directly e.g. like this (non-recursive):
```sh
clang-format -i src/*.{h,c}
```

But, it's better to use git hooks.

## Git Hooks
Float Accessories uses `lefthook` for managing git hooks. Again, you need to install this dependency in your development environment. Then run this to install the commit hook in the repo:
```sh
lefthook install
```

After this, `lefthook` will automatically run clang-format on every commit you make and check your formatting.

To auto-fix all formatting of all files in your working tree, run:
```sh
lefthook run clang-format-fix
```

## Naming Conventions
- Type names are in `PascalCase`.
- Global constants and enum items are in `SCREAMING_CASE`.
- Variables (local, global, struct members) and functions are in `snake_case`.
- File names are in `snake_case.c`.
