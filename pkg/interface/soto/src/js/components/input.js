import React, { Component } from 'react';
import { store } from '../store';
import { api } from '../api';

export class Input extends Component {
  constructor(props) {
    super(props);
    this.keyPress = this.keyPress.bind(this);
    this.inputRef = React.createRef();
  }

  componentDidUpdate() {
      this.inputRef.current.setSelectionRange(this.props.cursor, this.props.cursor);
    }
  
  keyPress = (e) => {
    e.preventDefault();

    let ignoredKeys = ["Meta", "Alt", "Control", "Escape", "Shift",
                       "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8",
                       "F9", "F10", "F11", "F12", "Backspace"
                      ];

  // submit on enter
  if (e.key === "Enter") {
    api.soto("ret");
  }

  else if ((e.key === "Backspace") && (this.props.cursor > 0)) {
    store.doEdit({ del: this.props.cursor - 1});
    return store.setState({ cursor: this.props.cursor - 1});
  }

  else if (e.key.startsWith("Arrow")) {

    if (e.key === "ArrowLeft") {
      if (this.props.cursor > 0) {
        store.setState({ cursor: this.props.cursor - 1 });
      }
    }

    else if (e.key === "ArrowRight") {
      if (this.props.cursor < this.props.input.length) {
        store.setState({ cursor: this.props.cursor + 1});
      }
    }

  }
  
  // tab completion
  else if (e.key === "Tab") {
    api.soto({tab: this.props.cursor});
  }

  // capture and transmit most characters
  else if (ignoredKeys.indexOf(e.key) === -1) {
    store.doEdit({ ins: { cha: e.key, at: this.props.cursor } });
    store.setState({ cursor: this.props.cursor + 1 });
  }
}

render() {
  return (
    <div className="flex flex-row flex-grow-1">
      <div className="flex-shrink-0">~{this.props.ship}:dojo
      </div>
      <span id="prompt">
        {this.props.prompt}
      </span>
      <input 
        autoCorrect="false" 
        autoFocus={true}
        className="mono ml1 flex-auto dib w-100"
        cursor={this.props.cursor}
        onKeyDown={this.keyPress}
        onPaste={e => {e.preventDefault()}}
        ref={this.inputRef}
        defaultValue={this.props.input}
      />
    </div>
    )
  }
}

export default Input;