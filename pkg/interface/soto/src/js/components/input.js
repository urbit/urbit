import React, { Component } from 'react';
import { store } from '../store';
import { api } from '../api';
import { cite } from '../lib/util';

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
    if ((e.getModifierState("Control") || event.getModifierState("Meta"))
       && e.key === "v") {
      return;
    }

    e.preventDefault();

    let allowedKeys = [
      "Enter", "Backspace", "ArrowLeft", "ArrowRight", "Tab"
    ];

    if ((e.key.length > 1) && (!(allowedKeys.includes(e.key)))) {
      return;
    }

  // submit on enter
  if (e.key === "Enter") {
    store.setSpinner(true);
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
    store.setSpinner(true);
    api.soto({tab: this.props.cursor});
  }

  // capture and transmit most characters
  else {
    store.doEdit({ ins: { cha: e.key, at: this.props.cursor } });
    store.setState({ cursor: this.props.cursor + 1 });
  }
}

render() {
  return (
    <div className="flex flex-row flex-grow-1">
      <div className="flex-shrink-0">{cite(this.props.ship)}:dojo
      </div>
      <span id="prompt">
        {this.props.prompt}
      </span>
      <input
        autoCorrect="false"
        autoFocus={true}
        className="mono ml1 flex-auto dib w-100"
        cursor={this.props.cursor}
        onClick={e => store.setState({ cursor: e.target.selectionEnd })}
        onKeyDown={this.keyPress}
        onPaste={e => {
          let clipboardData = e.clipboardData || window.clipboardData;
          let paste = Array.from(clipboardData.getData('Text'));
          paste.reduce(async (previous, next) => {
            await previous;
            this.setState({cursor: this.props.cursor + 1});
            return store.doEdit({ ins: { cha: next, at: this.props.cursor } });
          }, Promise.resolve());
          e.preventDefault();
          }}
        ref={this.inputRef}
        defaultValue={this.props.input}
      />
    </div>
    )
  }
}

export default Input;
