import React from 'react';
import { Link } from 'react-router-dom';
import defaultApps from '~/logic/lib/default-apps';

import { Box, DisclosureBox } from "@tlon/indigo-react";
const routeList = defaultApps.map(a => `/~${a}`);

export default class Tile extends React.Component {
  render() {
    const { bg, to, href, p, boxShadow, ...props } = this.props;

    let childElement = (
      <Box p={typeof p === 'undefined' ? 2 : p} width="100%" height="100%">
        {props.children}
      </Box>
    );

    if (to) {
      if (routeList.indexOf(to) !== -1 || to === '/~landscape/home' || to === '/~profile' || to.startsWith('/~landscape/ship')) {
        childElement= (<Link to={to}>{childElement}</Link>);
      } else {
        childElement= (<a href={to}>{childElement}</a>);
      }

    }


    return (
      <Box
        borderRadius={2}
        overflow="hidden"
        bg={bg || "white"}
        color='washedGray'
        boxShadow={boxShadow || '0 0 0px 1px inset'}
        {...props}
      >
        <Box
          height="100%"
          width="100%"
        >
          {childElement}
        </Box>
      </Box>
    );
  }
}
