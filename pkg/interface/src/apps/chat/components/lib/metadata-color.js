import React, { Component } from 'react';

import { uxToHex } from '../../../../lib/util';


export class MetadataColor extends Component {

  constructor(props) {
    super(props);

    this.state = {
      value: props.initialValue
    };
    
    this.inputRef = React.createRef();
  }

  componentDidUpdate(prevProps) {
    const { props } = this;
    if (prevProps.initialValue !== props.initialValue) {
      this.setState({ value: props.initialValue }, () => {
        if (this.inputRef.current) {
          this.inputRef.current.value =
            `#${uxToHex(props.initialValue)}`;
        }
      });
    }
  }

  changeColor() {
    this.setState({ color: event.target.value });
  }

  submitColor() {
    const { props, state } = this;

    let color = state.color;
    if (color.startsWith('#')) {
      color = state.color.substr(1);
    }
    const hexExp = /([0-9A-Fa-f]{6})/;
    const hexTest = hexExp.exec(color);
    let currentColor = '000000';
    if (props.association && 'metadata' in props.association) {
      currentColor = uxToHex(props.association.metadata.color);
    }
    if (hexTest && (hexTest[1] !== currentColor)) {
      const chatOwner = (deSig(props.match.params.ship) === window.ship);
      const association =
        (props.association) && ('metadata' in props.association)
          ? props.association : {};

      if (chatOwner) {
        this.setState({ awaiting: true, type: 'Editing chat...' }, (() => {
          props.api.metadata.metadataAdd(
            'chat',
            association['app-path'],
            association['group-path'],
            association.metadata.title,
            association.metadata.description,
            association.metadata['date-created'],
            color
          ).then(() => {
            this.setState({ awaiting: false });
          });
        }));
      }
    }
  }

  render() {
    return (
      <div>
        <p className="f8 mt3 lh-copy">Change color</p>
        <p className="f9 gray2 db mb4">Give this chat a color when viewing group channels</p>
        <div className="relative w-100 flex"
          style={{ maxWidth: '10rem' }}
        >
          <div className="absolute"
            style={{
              height: 16,
              width: 16,
              backgroundColor: state.color,
              top: 13,
              left: 11
              }}
          />
          <input
            className={'pl7 f8 ba b--gray3 b--gray2-d bg-gray0-d white-d ' +
              'focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3'}
            value={state.color}
            disabled={!chatOwner}
            onChange={this.changeColor}
            onBlur={this.submitColor}
          />
        </div>
      </div>
    );
  }
}
