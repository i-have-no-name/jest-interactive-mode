# jest-interactive-mode

emacs jest tool with immediately highlighting results

![](./jest-interactive-mode.gif)

## Usage

### Commands

* `jest-interactive-mode`: Start jest in current file
* `jest-interactive-display-list-errors`: Show failed tests

## Screenshots

![](./docs/screenshots/modeline-success.png)
![](./docs/screenshots/modeline-failed.png)
![](./docs/screenshots/list-errors.png)

## Caveats

It's works for one test for now. When you finish with test file you should turn off mode and run for another. 
(fix it in the future)

## TODO
* [ ] Display `console.log` 
* [ ] Loading spinner on initialization and recomputing
* [x] Align text in `display-list-errors` 
