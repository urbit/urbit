
import React, { Component, useRef } from 'react';
import { BaseInput } from '@tlon/indigo-react';

const OmniboxInput = (props) => {
  const input = useRef<HTMLInputElement>();
  return (
    <BaseInput
      ref={(el) => {
        if (!el) return;
        input.current = el;
          if (el && document.activeElement.isSameNode(el)) {
            el.blur();
            el.focus();
          }
        }
      }
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

export default OmniboxInput;

