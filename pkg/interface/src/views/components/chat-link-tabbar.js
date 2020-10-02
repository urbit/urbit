import React from 'react';
import { Link } from 'react-router-dom';

import { Box } from '@tlon/indigo-react';

export const TabBar = (props) => {
  const {
    location,
    settings,
  } = props;
  let setColor = '';

  if (location.pathname.includes('/settings')) {
    setColor = 'black white-d';
  } else {
    setColor = 'gray3';
  }

  return (
    <Box display='inline-block' flexShrink='0' flexGrow='1'>
      <Box display='inline-block' pt='9px' fontSize='0' pl='16px' pr='6'>
        <Link
          className={'no-underline ' + setColor}
          to={settings}>
          Settings
        </Link>
      </Box>
    </Box>
  );
};
