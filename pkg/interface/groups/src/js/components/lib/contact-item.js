import React, { Component } from 'react';
import { Route, Link } from 'react-router-dom';
import { Sigil } from '../lib/icons/sigil';
import { uxToHex, cite } from '../../lib/util';


export class ContactItem extends Component {
  render() {
    const { props } = this;

    let selectedClass = (props.selected) ? "bg-gray4 bg-gray1-d" : "";
    let hexColor = uxToHex(props.color);
    let name = (props.nickname) ? props.nickname : cite(props.ship);

    let prefix = props.share ? 'share' : 'view';
    let suffix = !props.share ? `/${props.ship}` : '';
    return (
      <Link to={`/~groups/${prefix}` + props.path + suffix}>
        <div className=
          {"pl4 pt1 pb1 f9 flex justify-start content-center " + selectedClass}
        >
          <Sigil
            ship={props.ship}
            color={"#" + hexColor}
            size={32}
            key={`${props.ship}.sidebar.${hexColor}`} />
          <p
            className={
              "f9 w-70 dib v-mid ml2 nowrap " +
              ((props.nickname) ? "" : "mono")}
            style={{ paddingTop: 6 }}
            title={props.ship}>
            {name}
          </p>
        </div>
      </Link>
    );
  }
}

