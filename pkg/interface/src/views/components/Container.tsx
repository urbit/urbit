import { allSystemStyle } from '@tlon/indigo-react';
import { css, SystemStyleObject } from '@styled-system/css';
import styled from 'styled-components';

const style = ({ borderWidth, round = false, expand = false, gapY }) => {
  const expandRules = expand ? { width: '100%', height: '100%' } : {};
  return css({
    ...expandRules,
    borderRadius: round ? 2 : 1,
    borderColor: 'lightGray',
    borderWidth: typeof borderWidth === 'undefined' ? 1 : borderWidth,
    borderStyle: 'solid',
    backgroundColor: 'white',
    display: 'flex',
    flexDirection: 'column',
    '& > *': typeof gapY === 'undefined' ? {} : { marginTop: gapY },
    '& > :first-child': typeof gapY === 'undefined' ? {} : { marginTop: 0 }
  } as SystemStyleObject);
};

export const Container = styled.div(style, ...allSystemStyle);
