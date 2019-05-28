import React, { Component } from 'react';
import { Route, Link } from "react-router-dom";
import classnames from 'classnames';


export class ChatTabBar extends Component {

  render() {
    let toBaseLink = '/~chat/' + this.props.station;

    let bbStream = '',
      bbMembers = '',
      bbSettings = '';

    let strColor = '',
      memColor = '',
      setColor = '';

    if (this.props.location.pathname.includes('/settings')) {
      bbSettings = ' bb';
      strColor = 'gray';
      memColor =  'gray';
      setColor = 'black';
    } else if (this.props.location.pathname.includes('/members')) {
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

    return (
      <div className="w-100" style={{ height:28 }}>
        <div className={"dib w-20 h-100" + bbStream}>
          <Link 
            className={'no-underline label-regular v-mid ' + strColor}
            to={toBaseLink}>Stream</Link>
        </div>
        <div className={"dib w-20 h-100" + bbMembers}>
          <Link
            className={'no-underline label-regular v-mid ' + memColor}
            to={toBaseLink + '/members'}>32 Members</Link>
        </div>
        <div className={"dib w-20 h-100" + bbSettings}>
          <Link
            className={'no-underline label-regular v-mid ' + setColor}
            to={toBaseLink + '/settings'}>Settings</Link>
        </div>
      </div>
    );
  }
}
