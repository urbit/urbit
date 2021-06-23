import { allSystemStyle } from '@tlon/indigo-react';
import { css, SystemStyleObject } from '@styled-system/css';
import styled from 'styled-components';

const style = ({ border, round = false, expand = false }) => {
  const expandRules = expand ? { width: '100%', height: '100%' } : {};
  return css({
    ...expandRules,
    borderRadius: round ? 2 : 1,
    border: typeof border === 'undefined' ? 1 : border,
    borderColor: 'lightGray',
    backgroundColor: 'white',
    display: 'flex',
    flexDirection: 'column'
  } as SystemStyleObject);
};

export const Container = styled.div(style, ...allSystemStyle);
