import React from 'react';
import { Link } from 'react-router-dom';

export const TabBar = (props) => {
  const {
    location,
    settings,
    popoutHref
  } = props;
  let setColor = '', popout = '';

  if (location.pathname.includes('/settings')) {
    setColor = 'black white-d';
  } else {
    setColor = 'gray3';
  }

  const hidePopoutIcon = (popout)
    ? 'dn-m dn-l dn-xl' : 'dib-m dib-l dib-xl';

  return (
    <div className="dib flex-shrink-0 flex-grow-1">
      <div className={'dib pt2 f9 pl6 pr6 lh-solid'}>
        <Link
          className={'no-underline ' + setColor}
          to={settings}>
          Settings
        </Link>
      </div>
      <a href={popoutHref} rel="noopener noreferrer"
         target="_blank"
         className="dib fr pr1"
         style={{ paddingTop: '8px' }}>
        <img
          className={'flex-shrink-0 pr3 dn ' + hidePopoutIcon}
          src="/~chat/img/popout.png"
          height="16"
          width="16" />
      </a>
    </div>
  );
};
