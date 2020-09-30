import React, { Component } from 'react';

export class MetadataColor extends Component {
  constructor(props) {
    super(props);
    this.state = {
      color: props.initialValue
    };

    this.changeColor = this.changeColor.bind(this);
    this.submitColor = this.submitColor.bind(this);
  }

  componentDidUpdate(prevProps) {
    const { props } = this;
    if (prevProps.initialValue !== props.initialValue) {
      this.setState({ color: props.initialValue });
    }
  }

  changeColor(event) {
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
    if (!props.isDisabled && hexTest && (state.color !== props.initialValue)) {
      props.setValue(color);
    }
  }

  render() {
    const { props, state } = this;
    return (
      <div className={'cf w-100 mb3 ' + ((props.isDisabled) ? 'o-30' : '')}>
        <p className="f8 lh-copy">Change color</p>
        <p className="f9 gray2 db mb4">Give this {props.resource} a color when viewing group channels</p>
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
            }} />
          <input
            className={'pl7 f8 ba b--gray3 b--gray2-d bg-gray0-d white-d ' +
              'focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3'}
            value={state.color}
            disabled={props.isDisabled}
            onChange={this.changeColor}
            onBlur={this.submitColor} />
        </div>
      </div>
    );
  }
}
