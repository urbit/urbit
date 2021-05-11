import { Box, BoxProps } from '@tlon/indigo-react';
import React, { RefObject } from 'react';
import { Link } from 'react-router-dom';
import styled from 'styled-components';
import { PaddingProps } from 'styled-system';
import defaultApps from '~/logic/lib/default-apps';

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

type TileProps = BoxProps & {
  bg?: string;
  to?: string;
  href?: string;
  p?: PaddingProps;
  children: any;
  gridColumnStart?: number;
  color?: string;
  className?: string;
}

const Tile = React.forwardRef((props: TileProps, ref: RefObject<HTMLDivElement>) => {
  const { bg, to, href, p, boxShadow, gridColumnStart, className = '', ...rest } = props;

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
      bg={bg || 'white'}
      color={props?.color || 'lightGray'}
      boxShadow={boxShadow || '0 0 0px 1px inset'}
      style={{ gridColumnStart }}
      className={`${className} tile`}
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
