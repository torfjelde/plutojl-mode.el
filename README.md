# Pluto.jl edit mode for Emacs

This Emacs minor mode allows provides you with some minor convenience functionality when editing `.jl` files compatible with Pluto.jl.

## Installation

### Prerequisites

Before installing the Pluto.jl Notebook Mode, please make sure you have the following dependencies installed:

- [Emacs](https://www.gnu.org/software/emacs/) (version 26 or higher)
- [Julia](https://julialang.org/) (version 1.0 or higher)
- [Pluto.jl](https://github.com/fonsp/Pluto.jl) (version 0.15.0 or higher)

### Using straight.el

To install the Pluto.jl Notebook Mode using [straight.el](https://github.com/raxod502/straight.el), follow these steps:

1. Open your Emacs configuration file (usually `~/.emacs.d/init.el` or `~/.emacs`) and add the following code:

   ```emacs-lisp
   (use-package plutojl-mode :straight (plutojl-mode :type git :host github :repo "torfjelde/plutojl-mode.el"))
   ```

2. Restart Emacs or evaluate the configuration by running `M-x eval-buffer`.

## Usage

Once you have installed the `plutojl-mode`, you can start editing Pluto.jl notebooks using Emacs.

### Enabling the Mode

To enable the Pluto.jl Notebook Mode, add the following line to your Emacs configuration file:

```emacs-lisp
(plutojl-mode)
```

Alternatively, you can enable the mode for specific file types using `auto-mode-alist`. For example, to enable the mode for files with the `.jl` extension, add the following to your configuration:

```emacs-lisp
(add-to-list 'auto-mode-alist '("\\.jl\\'" . plutojl-mode))
```

### Keybindings

- `plutojl-insert-cell-at-point` (`C-c C-c`): Insert a new Pluto.jl notebook cell at the current point.

### Interacting with Pluto.jl

If you are going to be editing Pluto notebooks in Emacs, then it's generally a good idea to start the Pluto.jl server with `auto_reload_from_file=true`, for example starting server using the following

``` julia
Pluto.run(Pluto.Configuration.from_flat_kwargs(auto_reload_from_file=true))
```

Then the notebook will update whenever you save changes in the `.jl` file.

## Contributing

Contributions are welcome! If you encounter any issues or have suggestions for improvements, please open an issue or submit a pull request on the [GitHub repository](https://github.com/torfjelde/plutojl-mode.el).

Please make sure to follow the [contribution guidelines](CONTRIBUTING.md) when submitting pull requests.

## License

This project is licensed under the [MIT License](LICENSE).
