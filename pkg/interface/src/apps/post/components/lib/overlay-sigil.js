import React, { Component } from 'react';
import { Sigil } from '../../../../lib/sigil';

export class OverlaySigil extends Component {
  constructor() {
    super();
  }

  render() {
    const { props, state } = this;

    return (
      <div
        className={props.className + ' pointer relative'}
        style={{ height: '24px' }}
      >
      <Sigil
        ship={props.ship}
        size={24}
        color={'000'}
        classes={props.sigilClass}
        />

      </div>
    );
  }
}
