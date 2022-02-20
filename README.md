# CanvasToTXT

## Building

This project uses Nix (you will need to enable flakes) and you can accelerate builds by using the binary cache `programmerino` with the command:
```
cachix use programmerino
```

and ultimately build with `nix build` which will place a binary in `result` or `nix build .#nuget` which will place a binary in `result-nuget`

## Usage

Pass the quiz page source code over stdin and the script will output the quiz questions and answers in a textual format. On Firefox, the [Copy HTML Text WE](https://addons.mozilla.org/en-US/firefox/addon/copy-html-text-we/) addon can be used to copy page HTML to clipboard.