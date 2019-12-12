import React, { Component } from 'react';
import { Route, Link } from 'react-router-dom';
import { Sigil } from '../lib/icons/sigil';
import { uxToHex } from '../../lib/util';


export class ContactItem extends Component {
  render() {
    const { props } = this;
    
    let selectedClass = (props.selected)
    ? "bg-gray4"
    : "";

    let hexColor = uxToHex(props.color);

    return (
      <Link
      to={"/~contacts" + props.path}>
        <div className={"pl4 pt1 pb1 f9 flex justify-start content-center " + selectedClass}>
          <Sigil ship={props.ship} color={"#" + hexColor} size={32}/>
        <p className="f9 w-70 dib v-mid ml2 nowrap mono"
           style={{paddingTop: 6}}>~{props.ship}</p>
        </div>
      </Link>
    )
  }
}

export default ContactItem;
