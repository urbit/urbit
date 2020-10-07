import React from 'react';
import { Link } from 'react-router-dom';
import defaultApps from '~/logic/lib/default-apps';

import { Box } from "@tlon/indigo-react";

export default class Tile extends React.Component {
  render() { 
    const { bg, to, href, p, ...props } = this.props;

    let childElement = (
      <Box p={typeof p === 'undefined' ? 2 : p} width="100%" height="100%">
        {props.children}
      </Box>
    );

    if (to) {
      if (routeList.indexOf(to) !== -1 || to === '/~landscape/home' || to === '/~profile') {
        childElement= (<Link to={to}>{childElement}</Link>);
      } else {
        childElement= (<a href={to}>{childElement}</a>);
      }

    }
      

    return (
      <Box
        border={1}
        borderRadius={2}
        borderColor="lightGray"
        overflow="hidden"
        {...props}
      >
        {childElement}
      </Box>
    );
  }
}
