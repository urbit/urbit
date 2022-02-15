import { Button, Icon, Row as _Row } from '@tlon/indigo-react';
import React from 'react';
import styled from 'styled-components';

const Row = styled(_Row)`
  cursor: pointer;
`;

type StatusBarItemProps = Parameters<typeof Row>[0] & { badge?: boolean; float?: boolean; };

export function StatusBarItem({
  badge,
  children,
  float,
  ...props
}: StatusBarItemProps) {
  const floatPos = float ? { zIndex: 10, boxShadow: 'rgba(0,0,0,0.2) 0px 0px 0px 999px' } : {};
  return (
    <Button
      style={{ position: 'relative', ...floatPos }}
      border={1}
      color="lightGray"
      bg="white"
      px={2}
      overflow='visible'
      zIndex={10}
      {...props}
    >
      {children}
      {badge && (
        <Icon
          size="22px"
          icon="Bullet"
          color="blue"
          style={{ position: 'absolute', top: '-10', right: '-11' }}
        />
      )}
    </Button>
  );
}
