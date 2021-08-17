import React from 'react';
import { Link } from 'react-router-dom';
import styled from 'styled-components';

import defaultApps from '~/logic/lib/default-apps';

import { Box } from "@tlon/indigo-react";

const SquareBox = styled(Box)`
  &::before {
    content: "";
    display: inline-block;
    width: 1px;
    height: 0;
    padding-bottom: 100%;
  }
  & > * {
    position: absolute;
    top: 0;
  }
  position: relative;
`;
const routeList = defaultApps.map(a => `/~${a}`);

const Tile = React.forwardRef((props, ref) => {
  const { bg, to, href, p, boxShadow, gridColumnStart, ...rest } = props;

  let childElement = (
    <Box p={typeof p === 'undefined' ? 2 : p} width="100%" height="100%">
      {props.children}
    </Box>
  );

  if (to) {
    if (routeList.indexOf(to) !== -1 || to === '/~profile' || to.startsWith('/~landscape/')) {
      childElement= (<Link to={to}>{childElement}</Link>);
    } else {
      childElement= (<a href={to}>{childElement}</a>);
    }

  }


  return (
    <SquareBox
      ref={ref}
      position="relative"
      borderRadius={2}
      overflow="hidden"
      bg={bg || "white"}
      color={props?.color || 'lightGray'}
      boxShadow={boxShadow || '0 0 0px 1px inset'}
      style={{ gridColumnStart }}
    >
      <Box
        {...rest}
        height="100%"
        width="100%"
      >
        {childElement}
      </Box>
    </SquareBox>
  );
});

export default Tile;
