import React, { Component } from 'react';
import { Link } from 'react-router-dom';


export class ChannelItem extends Component {
  render() {
    const { props } = this;

    const selectedClass = (props.selected)
    ? 'bg-gray5 bg-gray1-d'
    : 'pointer hover-bg-gray5 hover-bg-gray1-d';

    const unseenCount = props.unseenCount > 0
      ? <span className="dib white bg-gray3 bg-gray2-d fw6 br1 absolute" style={{ padding: '1px 5px', right: 8 }}>{props.unseenCount}</span>
      : null;

    return (
      <Link to={`/~link/${props.link}`}>
        <div className={'w-100 v-mid f9 ph5 z1 pv1 relative ' + selectedClass}>
          <p className="f9 dib">{props.name}</p>
          <p className="f9 dib fr">
            {unseenCount}
          </p>
        </div>
      </Link>
    );
  }
}

