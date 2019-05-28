import React, { Component } from 'react';
import classnames from 'classnames';

export class SidebarItem extends Component {

  onClick() {
    const { props } = this;
    props.history.push('/~chat/' + props.cir);
  }

  render() {
    const { props } = this;

    let selectedCss = !!props.selected ? 'bg-light-gray' : 'bg-white pointer';
    return (
      <div className={'pa3 ' + selectedCss} onClick={this.onClick.bind(this)}>
        <div className='w-100 v-mid'>
          <p className="body-regular">{props.title}</p>
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

