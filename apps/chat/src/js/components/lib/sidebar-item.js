import React, { Component } from 'react';
import classnames from 'classnames';

export class SidebarItem extends Component {

  onClick() {
    const { props } = this;
    props.history.push('/~chat/' + props.cir);
  }

  render() {
    const { props } = this;

    let unreadElem = !!props.unread ? (
      <div
        className="bg-nice-green dib mr2"
        style={{ borderRadius: 6, width: 12, height: 12 }}>
      </div>
    ) : (
      <div className="dib"></div>
    );

    let selectedCss = !!props.selected ? 'bg-light-gray' : 'bg-white pointer';
    return (
      <div className={'pa3 ' + selectedCss} onClick={this.onClick.bind(this)}>
        <div className='w-100 v-mid'>
          {unreadElem}
          <p className="dib body-regular">{props.title}</p>
        </div>
        <div className="w-100">
          <p className='dib gray label-small-mono mr3'>{props.ship}</p>
          <p className='dib gray label-small-mono'>{props.datetime}</p>
        </div>
        <p className='body-regular-400 gray'>{props.description}</p>
      </div>
    )
  }
}

