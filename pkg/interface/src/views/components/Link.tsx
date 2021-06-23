import {
  BoxProps,
  ColProps,
  allSystemStyle,
  ActionProps,
  asAction,
  asButton,
  ButtonProps,
  TextProps,
  RowProps
} from '@tlon/indigo-react';
import React from 'react';
import styled from 'styled-components';
import { css } from '@styled-system/css';
import { Link, LinkProps } from 'react-router-dom';
import { compose } from 'styled-system';

export const BlockLink = styled(Link)`
  display: block;
`;

export const ActionLink = asAction(BlockLink) as React.FC<
  ActionProps & LinkProps
>;

export const ButtonLink = asButton(BlockLink) as React.FC<
  ButtonProps & LinkProps
>;

export const BoxLink = styled(BlockLink)<
  React.PropsWithChildren<LinkProps & BoxProps>
>(compose(...allSystemStyle));

export const colLinkStyle = ({ gapY }: ColProps) =>
  css({
    display: 'flex',
    flexDirection: 'column',
    '& > *': typeof gapY === 'undefined' ? {} : { marginTop: gapY },
    '& > :first-child': typeof gapY === 'undefined' ? {} : { marginTop: 0 }
  });

export const ColLink = styled(Link)<
  React.PropsWithChildren<LinkProps & ColProps>
>(colLinkStyle, ...allSystemStyle);

export const rowLinkStyle = ({ gapX }: RowProps) =>
  css({
    display: 'flex',
    '& > *': typeof gapX === 'undefined' ? {} : { marginRight: gapX },
    '& > :last-child': typeof gapX === 'undefined' ? {} : { marginRight: 0 }
  });

export const RowLink = styled(Link)<
  React.PropsWithChildren<LinkProps & RowProps>
>(rowLinkStyle, ...allSystemStyle);

RowLink.defaultProps = {
  alignItems: 'center'
};

export const TextLink = styled(Link)<
  React.PropsWithChildren<LinkProps & TextProps>
>(compose(...allSystemStyle));

TextLink.defaultProps = {
  color: 'black'
};
