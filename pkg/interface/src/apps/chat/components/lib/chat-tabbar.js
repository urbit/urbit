import React, { Component } from 'react';
import { Link } from 'react-router-dom';

export class ChatTabBar extends Component {
  render() {
    const props = this.props;

    let memColor = '',
      setColor = '',
      popout = '';

    if (props.location.pathname.includes('/settings')) {
      memColor =  'gray3';
      setColor = 'black white-d';
    } else if (props.location.pathname.includes('/members')) {
      memColor =  'black white-d';
      setColor = 'gray3';
    } else {
      memColor =  'gray3';
      setColor = 'gray3';
    }

    popout = props.location.pathname.includes('/popout')
      ? 'popout/' :  '';

    const hidePopoutIcon = (this.props.popout)
      ? 'dn-m dn-l dn-xl' : 'dib-m dib-l dib-xl';

    return (
      <div className="dib flex-shrink-0 flex-grow-1">
        {props.isOwner ? (
          <div className={'dib pt2 f9 pl6 lh-solid'}>
            <Link
              className={'no-underline ' + memColor}
              to={'/~chat/' + popout + 'members' + props.station}
            >
              Members
            </Link>
          </div>
        ) : (
          <div className="dib" style={{ width: 0 }}></div>
        )}
        <div className={'dib pt2 f9 pl6 pr6 lh-solid'}>
          <Link
            className={'no-underline ' + setColor}
            to={'/~chat/' + popout + 'settings' + props.station}
          >
            Settings
          </Link>
        </div>
        <a href={'/~chat/popout/room' + props.station} rel="noopener noreferrer"
        target="_blank"
        className="dib fr pr1"
        style={{ paddingTop: '5px' }}
        >
          <img
            className={'flex-shrink-0 pr3 dn ' + hidePopoutIcon}
            src="/~chat/img/popout.png"
            height="16"
            width="16"
          />
        </a>
      </div>
    );
  }
}
