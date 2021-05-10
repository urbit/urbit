import { BaseInput } from '@tlon/indigo-react';
import React, { Component } from 'react';

interface OmniboxInputProps {
  control: () => void;
  search: () => void;
  query: string;
}

export class OmniboxInput extends Component<OmniboxInputProps> {
  input: HTMLInputElement | null;
  render() {
    const { props } = this;
    return (
      <BaseInput
        ref={(el) => {
          this.input = el;
          if (el && document.activeElement.isSameNode(el)) {
            el.blur();
            el.focus();
          }
        }}
        width='100%'
        p='2'
        backgroundColor='white'
        color='black'
        border='1px solid transparent'
        borderRadius='2'
        maxWidth='calc(600px - 1.15rem)'
        fontSize='1'
        style={{ boxSizing: 'border-box' }}
        placeholder='Search...'
        onKeyDown={props.control}
        onChange={props.search}
        spellCheck={false}
        value={props.query}
      />
    );
  }
}

export default OmniboxInput;
