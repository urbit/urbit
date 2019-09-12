import React, { Component } from 'react';
import { Route, Link } from "react-router-dom";
import classnames from 'classnames';


export class ChatTabBar extends Component {

  render() {
    let props = this.props;
    let toBaseLink = '/~chat/room' + props.station;

    let bbStream = '',
      bbMembers = '',
      bbSettings = '';

    let strColor = '',
      memColor = '',
      setColor = '';

    if (props.location.pathname.includes('/settings')) {
      bbSettings = ' bb';
      strColor = 'gray';
      memColor =  'gray';
      setColor = 'black';
    } else if (props.location.pathname.includes('/members')) {
      bbMembers = ' bb';
      strColor = 'gray';
      memColor =  'black';
      setColor = 'gray';
    } else {
      bbStream = ' bb';
      strColor = 'black';
      memColor =  'gray';
      setColor = 'gray';
    }

    let membersText = props.numPeers === 1
      ? '1 Member' : `${props.numPeers} Members`;

    console.log(props.isOwner);

    return (
      <div className="w-100" style={{ height:28 }}>
        <div className={"dib h-100" + bbStream} style={{width:'160px'}}>
          <Link
            className={'no-underline label-regular v-mid ' + strColor}
            to={toBaseLink}>Stream</Link>
        </div>
        { !!props.isOwner ? (
          <div className={"dib h-100" + bbMembers} style={{width:'160px'}}>
            <Link
              className={'no-underline label-regular v-mid ' + memColor}
              to={toBaseLink + '/members'}>{membersText}</Link>
          </div>
          ) : <div className="dib" style={{width:0}}></div>
        }
        <div className={"dib h-100" + bbSettings} style={{width:'160px'}}>
          <Link
            className={'no-underline label-regular v-mid ' + setColor}
            to={toBaseLink + '/settings'}>Settings</Link>
        </div>
      </div>
    );
  }
}
