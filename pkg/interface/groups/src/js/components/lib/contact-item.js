import React, { Component } from 'react';
import { Route, Link } from 'react-router-dom';
import { Sigil } from '../lib/icons/sigil';
import { uxToHex, cite } from '../../lib/util';

export class ContactItem extends Component {
  render() {
    const { props } = this;

    const selectedClass = (props.selected) ? 'bg-gray4 bg-gray1-d' : '';
    const hexColor = uxToHex(props.color);
    const name = (props.nickname) ? props.nickname : cite(props.ship);

    const prefix = props.share ? 'share' : 'view';
    const suffix = !props.share ? `/${props.ship}` : '';

    const img = (props.avatar !== null)
      ? <img className="dib" src={props.avatar} height={32} width={32} />
      : <Sigil
        ship={props.ship}
        color={'#' + hexColor}
        size={32}
        key={`${props.ship}.sidebar.${hexColor}`}
        />;

    return (
      <Link to={`/~groups/${prefix}` + props.path + suffix}>
        <div className=
          {'pl4 pt1 pb1 f9 flex justify-start content-center ' + selectedClass}
        >
        {img}
          <p
            className={
              'f9 w-70 dib v-mid ml2 nowrap ' +
              ((props.nickname) ? '' : 'mono')}
            style={{ paddingTop: 6 }}
            title={props.ship}
          >
            {name}
          </p>
        </div>
      </Link>
    );
  }
}

