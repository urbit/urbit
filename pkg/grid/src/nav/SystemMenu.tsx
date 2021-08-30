import * as DropdownMenu from '@radix-ui/react-dropdown-menu';
import classNames from 'classnames';
import clipboardCopy from 'clipboard-copy';
import React, { HTMLAttributes, useCallback, useState } from 'react';
import { Link } from 'react-router-dom';
import { Adjust } from '../components/icons/Adjust';
import { disableDefault, handleDropdownLink } from '../state/util';
import { MenuState } from './Nav';

type SystemMenuProps = HTMLAttributes<HTMLButtonElement> & {
  open: boolean;
  setOpen: (open: boolean) => void;
  menu: MenuState;
  navOpen: boolean;
};

export const SystemMenu = ({ open, setOpen, className, menu, navOpen }: SystemMenuProps) => {
  const [copied, setCopied] = useState(false);

  const copyHash = useCallback((event: Event) => {
    event.preventDefault();

    setCopied(true);
    clipboardCopy('fjuhl');

    setTimeout(() => {
      setCopied(false);
    }, 1250);
  }, []);

  return (
    <>
      <DropdownMenu.Root open={open} onOpenChange={(isOpen) => setOpen(isOpen)}>
        <DropdownMenu.Trigger
          className={classNames(
            'circle-button default-ring',
            open && 'text-gray-300',
            navOpen && menu !== 'system-preferences' && menu !== 'help-and-support' && 'opacity-80',
            className
          )}
        >
          <Adjust className="w-6 h-6 fill-current" />
          <span className="sr-only">System Menu</span>
        </DropdownMenu.Trigger>

        <DropdownMenu.Content
          onCloseAutoFocus={disableDefault}
          sideOffset={12}
          className="dropdown min-w-64 p-4 font-semibold text-gray-500 bg-white"
        >
          <DropdownMenu.Group>
            <DropdownMenu.Item
              as={Link}
              to="/leap/system-preferences"
              className="flex items-center p-2 mb-2 space-x-2 focus:bg-blue-200 focus:outline-none rounded"
              onSelect={handleDropdownLink(setOpen)}
            >
              <span className="w-5 h-5 bg-gray-100 rounded-full" />
              <span className="h4">System Preferences</span>
            </DropdownMenu.Item>
            <DropdownMenu.Item
              as={Link}
              to="/leap/help-and-support"
              className="flex items-center p-2 mb-2 space-x-2 focus:bg-blue-200 focus:outline-none rounded"
              onSelect={handleDropdownLink(setOpen)}
            >
              <span className="w-5 h-5 bg-gray-100 rounded-full" />
              <span className="h4">Help and Support</span>
            </DropdownMenu.Item>
            <DropdownMenu.Item
              as="button"
              className="inline-flex items-center py-2 px-3 m-2 h4 text-black bg-gray-100 rounded focus:bg-blue-200 focus:outline-none"
              onSelect={copyHash}
            >
              <span className="sr-only">Base Hash</span>
              <code>
                {!copied && <span aria-label="f-j-u-h-l">fjuhl</span>}
                {copied && 'copied!'}
              </code>
            </DropdownMenu.Item>
          </DropdownMenu.Group>
        </DropdownMenu.Content>
      </DropdownMenu.Root>

      {!navOpen && open && (
        <div className="fixed z-30 right-0 bottom-0 w-screen h-screen bg-black opacity-30" />
      )}
    </>
  );
};
