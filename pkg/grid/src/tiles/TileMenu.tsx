import React, { useState } from 'react';
import type * as Polymorphic from '@radix-ui/react-polymorphic';
import * as DropdownMenu from '@radix-ui/react-dropdown-menu';
import classNames from 'classnames';
import { Link } from 'react-router-dom';
import { useMutation, useQueryClient } from 'react-query';
import { chargesKey, toggleDocket } from '../state/docket';

export interface TileMenuProps {
  desk: string;
  lightText: boolean;
  menuColor: string;
  active: boolean;
  className?: string;
}

const MenuIcon = ({ className }: { className: string }) => (
  <svg className={classNames('fill-current', className)} viewBox="0 0 16 16">
    <path fillRule="evenodd" clipRule="evenodd" d="M14 8.5H2V7.5H14V8.5Z" />
    <path fillRule="evenodd" clipRule="evenodd" d="M2 2.5H14V3.5H2V2.5Z" />
    <path fillRule="evenodd" clipRule="evenodd" d="M14 13.5H2V12.5H14V13.5Z" />
  </svg>
);

type ItemComponent = Polymorphic.ForwardRefComponent<
  Polymorphic.IntrinsicElement<typeof DropdownMenu.Item>,
  Polymorphic.OwnProps<typeof DropdownMenu.Item>
>;

const Item = React.forwardRef(({ children, ...props }, ref) => (
  <DropdownMenu.Item
    ref={ref}
    {...props}
    className="block w-full px-4 py-1 leading-none rounded mix-blend-hard-light select-none default-ring ring-gray-600"
  >
    {children}
  </DropdownMenu.Item>
)) as ItemComponent;

export const TileMenu = ({ desk, active, menuColor, lightText, className }: TileMenuProps) => {
  const queryClient = useQueryClient();
  const [open, setOpen] = useState(false);
  const { mutate } = useMutation(() => toggleDocket(desk), {
    onSuccess: () => {
      queryClient.invalidateQueries(chargesKey());
    }
  });

  const menuBg = { backgroundColor: menuColor };

  return (
    <DropdownMenu.Root open={open} onOpenChange={(isOpen) => setOpen(isOpen)}>
      <DropdownMenu.Trigger
        className={classNames(
          'flex items-center justify-center w-8 h-8 rounded-full transition-opacity duration-75 default-ring',
          open && 'opacity-100',
          className
        )}
        style={menuBg}
        // onMouseOver={() => queryClient.setQueryData(['apps', name], app)}
      >
        <MenuIcon
          className={classNames('w-4 h-4 mix-blend-hard-light', lightText && 'text-gray-100')}
        />
        <span className="sr-only">Menu</span>
      </DropdownMenu.Trigger>

      <DropdownMenu.Content
        align="start"
        alignOffset={-32}
        sideOffset={4}
        onCloseAutoFocus={(e) => e.preventDefault()}
        className={classNames(
          'dropdown font-semibold',
          lightText ? 'text-gray-100' : 'text-gray-800'
        )}
        style={menuBg}
      >
        <DropdownMenu.Group className="space-y-4">
          {/* 
            TODO: revisit with Liam
            <Item as={Link} to={`/leap/search/${provider}/apps/${name.toLowerCase()}`} onSelect={(e) => { e.preventDefault(); setTimeout(() => setOpen(false), 0) }}>App Info</Item> 
          */}
          <Item
            as={Link}
            to={`/app/${desk}`}
            onSelect={(e) => {
              e.preventDefault();
              setTimeout(() => setOpen(false), 0);
            }}
          >
            App Info
          </Item>
        </DropdownMenu.Group>
        <DropdownMenu.Separator className="-mx-4 my-2 border-t-2 border-solid border-gray-500 mix-blend-soft-light" />
        <DropdownMenu.Group className="space-y-4">
          {active && (
            <Item
              as={Link}
              to={`/app/${desk}/suspend`}
              onSelect={(e) => {
                e.preventDefault();
                setTimeout(() => setOpen(false), 0);
              }}
            >
              Suspend App
            </Item>
          )}
          {!active && <Item onSelect={() => mutate()}>Resume App</Item>}
          <Item
            as={Link}
            to={`/app/${desk}/remove`}
            onSelect={(e) => {
              e.preventDefault();
              setTimeout(() => setOpen(false), 0);
            }}
          >
            Remove App
          </Item>
        </DropdownMenu.Group>
        <DropdownMenu.Arrow
          className="w-4 h-[10px] fill-current -translate-x-10"
          style={{ color: menuColor }}
        />
      </DropdownMenu.Content>
    </DropdownMenu.Root>
  );
};
