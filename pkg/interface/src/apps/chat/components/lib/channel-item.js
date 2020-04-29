import React, { Component } from 'react';

export class ChannelItem extends Component {
  constructor(props) {
    super(props);
  }

  onClick() {
    const { props } = this;
    props.history.push('/~chat/room' + props.box);
  }

  render() {
    const { props } = this;

    const unreadElem = props.unread ? 'fw6' : '';

    const title = props.title;

    const selectedCss = props.selected
      ? 'bg-gray4 bg-gray1-d gray3-d c-default'
      : 'bg-white bg-gray0-d gray3-d hover-bg-gray5 hover-bg-gray1-d pointer';

    return (
      <div
        className={'z1 ph4 pv1 ' + selectedCss}
        onClick={this.onClick.bind(this)}
      >
        <div className="w-100 v-mid">
          <p className={'dib f9 ' + unreadElem}>
              {title}
          </p>
        </div>
      </div>
    );
  }
}
