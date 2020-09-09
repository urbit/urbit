import React, { Component } from 'react';

export class MetadataInput extends Component {

  constructor(props) {
    super(props);

    this.state = {
      value: props.initialValue
    };
  }

  componentDidUpdate(prevProps) {
    const { props } = this;
    if (prevProps.initialValue !== props.initialValue) {
      this.setState({ value: props.initialValue });
    }
  }

  render() {
    const {
      title,
      description,
      isDisabled,
      setValue
    } = this.props;

    return (
      <div className={'w-100 mb3 fl ' + ((isDisabled) ? 'o-30' : '')}>
        <p className="f8 lh-copy">{title}</p>
        <p className="f9 gray2 db mb4">{description}</p>
        <div className="relative w-100 flex" style={{ maxWidth: '29rem' }}>
          <input
            className={'f8 ba b--gray3 b--gray2-d bg-gray0-d white-d ' +
            'focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3'}
            type="text"
            value={this.state.value}
            disabled={isDisabled}
            onChange={(e) => {
              this.setState({ value: e.target.value });
            }}
            onBlur={() => {
              if (!isDisabled) {
                setValue(this.state.value || '');
              }
            }}
          />
        </div>
      </div>
    );
  }
}
