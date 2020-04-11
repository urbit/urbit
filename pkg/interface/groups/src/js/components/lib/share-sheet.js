import React, { Component } from 'react';
import { Route, Link } from 'react-router-dom';
import { Sigil } from '../lib/icons/sigil';
import { uxToHex } from '../../lib/util';
import { ContactItem } from '/components/lib/contact-item';


export class ShareSheet extends Component {
  render() {
    const { props } = this;
    let selectedClass = (props.selected) ? "bg-gray4" : "";
    let hexColor = uxToHex(props.color);

    return (
      <div>
        <p className="pt4 pb2 pl4 pr4 f8 gray2 f9">Group Identity</p>
        <ContactItem
          key={props.ship}
          ship={props.ship}
          nickname={props.nickname}
          color={props.color}
          path={props.path}
          selected={props.selected}
          share={true} />
        <p className="pt2 pb3 pl4 pr4 f9 white-d">
           Your personal information is hidden to others in this group
           by default.
        </p>
        <p className="pl4 pr4 f9 white-d">
          Share whenever you are ready, or edit its contents for this group.
        </p>
      </div>
    );
  }
}

