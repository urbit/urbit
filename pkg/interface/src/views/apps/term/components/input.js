import React, { Component } from 'react';
import { Row, Box, BaseInput } from '@tlon/indigo-react';
import { cite } from '~/logic/lib/util';
import { Spinner } from '~/views/components/Spinner';

export class Input extends Component {
  constructor(props) {
    super(props);
    this.state = {};
    this.keyPress = this.keyPress.bind(this);
    this.paste = this.paste.bind(this);
    this.click = this.click.bind(this);
    this.inputRef = React.createRef();
  }

  componentDidUpdate() {
    if (
      !document.activeElement == document.body
      || document.activeElement == this.inputRef.current
    ) {
      this.inputRef.current.focus();
      this.inputRef.current.setSelectionRange(this.props.cursor, this.props.cursor);
    }
  }

  keyPress(e) {
    let key = e.key;
    //  let paste and leap events pass
    if ((e.getModifierState('Control') || event.getModifierState('Meta'))
        && (e.key === 'v' || e.key === '/')) {
      return;
    }

    let belt = null;
         if (key === 'ArrowLeft')  belt = {aro: 'l'};
    else if (key === 'ArrowRight') belt = {aro: 'r'};
    else if (key === 'ArrowUp')    belt = {aro: 'u'};
    else if (key === 'ArrowDown')  belt = {aro: 'd'};
    else if (key === 'Backspace')  belt = {bac: null};
    else if (key === 'Delete')     belt = {del: null};
    else if (key === 'Tab')        belt = {ctl: 'i'};
    else if (key === 'Enter')      belt = {ret: null};
    else if (key.length === 1)     belt = {txt: [key]};
    else                           belt = null;

    if (belt && e.getModifierState('Control')) {
      if (belt.txt !== undefined) belt = {ctl: belt.txt[0]};
    } else
    if (belt &&
        (e.getModifierState('Meta') || e.getModifierState('Alt'))) {
      if (belt.bac !== undefined) belt = {met: 'bac'};
    }

    if (belt !== null) {
      this.props.api.belt(belt);
    }

    e.preventDefault();
  }

  paste(e) {
    const clipboardData = e.clipboardData || window.clipboardData;
    const clipboardText = clipboardData.getData('Text');
    this.props.api.belt({ txt: [...clipboardText] });
    e.preventDefault();
  }

  click(e) {
    // prevent desynced cursor movement
    e.preventDefault();
    e.target.setSelectionRange(this.props.cursor, this.props.cursor);
  }

  render() {
    const line = this.props.line;
    let prompt = 'connecting...';
    if (line) {
      if (line.lin) {
        prompt = line.lin.join('');
      }
      //TODO  render prompt style
      else if (line.klr) {
        prompt = line.klr.reduce((l, p) => (l + p.text.join('')), '');
      }
    }
    return (
      <Row flexGrow='1' position='relative'>
        <Box flexShrink='0' className="w-100">
          <BaseInput
            autoFocus
            autoCorrect="off"
            autoCapitalize="off"
            spellCheck="false"
            tabindex="0"
            wrap="off"
            className="mono ml1 flex-auto dib w-100"
            id="term"
            cursor={this.props.cursor}
            onKeyDown={this.keyPress}
            onClick={this.click}
            onPaste={this.paste}
            ref={this.inputRef}
            defaultValue="connecting..."
            value={prompt}
          />
        </Box>
      </Row>
    );
  }
}

export default Input;
