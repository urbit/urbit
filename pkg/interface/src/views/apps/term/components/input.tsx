import { BaseInput, Box, Row } from '@tlon/indigo-react';
import React, { Component } from 'react';

export class Input extends Component<any, {}> {
  inputRef: React.RefObject<unknown>;
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
      document.activeElement == this.inputRef.current
    ) {
      // @ts-ignore ref type issues
      this.inputRef.current.focus();
      // @ts-ignore ref type issues
      this.inputRef.current.setSelectionRange(this.props.cursor, this.props.cursor);
    }
  }

  keyPress(e) {
    const key = e.key;
    //  let paste and leap events pass
    if ((e.getModifierState('Control') || e.getModifierState('Meta'))
        && (e.key === 'v' || e.key === '/')) {
      return;
    }

    let belt = null;
         if (key === 'ArrowLeft')
belt = { aro: 'l' };
    else if (key === 'ArrowRight')
belt = { aro: 'r' };
    else if (key === 'ArrowUp')
belt = { aro: 'u' };
    else if (key === 'ArrowDown')
belt = { aro: 'd' };
    else if (key === 'Backspace')
belt = { bac: null };
    else if (key === 'Delete')
belt = { del: null };
    else if (key === 'Tab')
belt = { ctl: 'i' };
    else if (key === 'Enter')
belt = { ret: null };
    else if (key.length === 1)
belt = { txt: [key] };
    else
belt = null;

    if (belt && e.getModifierState('Control')) {
      if (belt.txt !== undefined)
belt = { ctl: belt.txt[0] };
    } else
    if (belt &&
        (e.getModifierState('Meta') || e.getModifierState('Alt'))) {
      if (belt.bac !== undefined)
belt = { met: 'bac' };
    }

    if (belt !== null) {
      this.props.api.belt(belt);
    }

    e.preventDefault();
  }

  paste(e) {
    const clipboardData = e.clipboardData || (window as any).clipboardData;
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
      } else if (line.klr) {
        // TODO  render prompt style
        prompt = line.klr.reduce((l, p) => (l + p.text.join('')), '');
      }
    }
    return (
      <Row flexGrow={1} position='relative'>
        <Box flexShrink={0} width='100%' color='black' fontSize={0}>
          <BaseInput
            autoFocus
            autoCorrect="off"
            autoCapitalize="off"
            color='black'
            minHeight={0}
            display='inline-block'
            width='100%'
            spellCheck="false"
            tabindex={0}
            wrap="off"
            className="mono"
            id="term"
            cursor={this.props.cursor}
            onKeyDown={this.keyPress}
            onClick={this.click}
            onPaste={this.paste}
            // @ts-ignore indigo-react doesn't let us pass refs
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
