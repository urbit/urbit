import React from "react";
import { Link } from "react-router-dom";
import styled from "styled-components";
import { Box } from "@tlon/indigo-react";
import { PropFunc } from "~/types/util";
interface HoverBoxProps {
  selected: boolean;
  bg: string;
  bgActive: string;
}
export const HoverBox = styled(Box)<HoverBoxProps>`
  background-color: ${(p) =>
    p.selected ? p.theme.colors[p.bgActive] : p.theme.colors[p.bg]};
  pointer: cursor;
  &:hover {
    background-color: ${(p) => p.theme.colors[p.bgActive]};
  }
`;

interface HoverBoxLinkProps {
  to: string;
}

export const HoverBoxLink = ({
  to,
  children,
  ...rest
}: HoverBoxLinkProps & PropFunc<typeof HoverBox>) => (
  <Link to={to}>
    <HoverBox {...rest}>{children}</HoverBox>
  </Link>
);
