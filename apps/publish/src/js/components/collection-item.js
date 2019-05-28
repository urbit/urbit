import React, { Component } from 'react';
import classnames from 'classnames';

export class CollectionItem extends Component {

  onClick() {
    const { props } = this;
    console.log("collection-item clicked!");
//    props.history.push(props.url);
  }

  render() {
    const { props } = this;

    let selectedCss = !!props.selected ? 'bg-light-gray' : 'bg-white pointer';
    return (
      <div className={'pa3 ' + selectedCss} onClick={this.onClick.bind(this)}>
        <div className='w-100 v-mid'>
          <h3 className='w-60 dib sans-serif'>{props.title}</h3>
        <p className='w-40 tr dib sans-serif gray'>{props.datetime}</p>
        </div>
        <p className='pt2 sans-serif gray'>{props.description}</p>
      </div>
    )
  }
}

