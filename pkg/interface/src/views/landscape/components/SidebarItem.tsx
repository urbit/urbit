import { Icon, Row, Text } from '@tlon/indigo-react';
import React, { ReactElement } from 'react';
import { IconRef, PropFunc } from '~/types/util';
import { HoverBoxLink } from '~/views/components/HoverBox';

interface SidebarItemProps {
  selected?: boolean;
  icon: IconRef;
  text: string;
  to: string;
  color?: string;
  children?: JSX.Element;
}

export const SidebarItem = ({
  icon,
  text,
  to,
  selected = false,
  color = 'black',
  children,
  ...rest
}: SidebarItemProps & PropFunc<typeof HoverBoxLink>): ReactElement => {
  return (
    <HoverBoxLink
      to={to}
      selected={selected}
      bg="white"
      bgActive="washedGray"
      display="flex"
      px={3}
      py={2}
      justifyContent="space-between"
      {...rest}
    >
      <Row alignItems="center" className='sidebar-item'>
        <Icon color={color} icon={icon as any} mr={2} />
        <Text color={color}>{text}</Text>
      </Row>
      {children}
    </HoverBoxLink>
  );
};
