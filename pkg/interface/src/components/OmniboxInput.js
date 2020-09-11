
import React, { Component } from 'react';

export class OmniboxInput extends Component {
  render() {
    const { props } = this;
    return (
    <input
      ref={(el) => {
        this.input = el;
        }
      }
      className='ba b--transparent w-100 br2 white-d bg-gray0-d inter f9 pa2'
      style={{ maxWidth: 'calc(600px - 1.15rem)', boxSizing: 'border-box' }}
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

