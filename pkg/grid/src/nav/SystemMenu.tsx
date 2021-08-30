import * as DropdownMenu from '@radix-ui/react-dropdown-menu';
import classNames from 'classnames';
import clipboardCopy from 'clipboard-copy';
import React, { HTMLAttributes, useCallback, useState } from 'react';
import { Link } from 'react-router-dom';
import { Adjust } from '../components/icons/Adjust';
import { disableDefault } from '../state/util';

type SystemMenuProps = HTMLAttributes<HTMLButtonElement> & {
  open: boolean;
  setOpen: (open: boolean) => void;
  showOverlay?: boolean;
};

export const SystemMenu = ({ open, setOpen, className, showOverlay = false }: SystemMenuProps) => {
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
          className={classNames('circle-button default-ring', open && 'text-gray-300', className)}
        >
          <Adjust className="w-6 h-6 fill-current" />
          <span className="sr-only">System Menu</span>
        </DropdownMenu.Trigger>

        <DropdownMenu.Content
          onCloseAutoFocus={disableDefault}
          sideOffset={12}
          className="dropdown min-w-64 p-6 font-semibold text-gray-500 bg-white"
        >
          <DropdownMenu.Group className="space-y-6">
            <DropdownMenu.Item
              as={Link}
              to="/leap/system-preferences"
              className="flex items-center space-x-2 default-ring ring-offset-2 rounded"
              onSelect={(e) => {
                e.preventDefault();
                setTimeout(() => setOpen(false), 0);
              }}
            >
              <span className="w-5 h-5 bg-gray-100 rounded-full" />
              <span className="h4">System Preferences</span>
            </DropdownMenu.Item>
            <DropdownMenu.Item
              as={Link}
              to="/leap/help-and-support"
              className="flex items-center space-x-2 default-ring ring-offset-2 rounded"
              onSelect={(e) => {
                e.stopPropagation();
                e.preventDefault();
                setTimeout(() => setOpen(false), 0);
              }}
            >
              <span className="w-5 h-5 bg-gray-100 rounded-full" />
              <span className="h4">Help and Support</span>
            </DropdownMenu.Item>
            <DropdownMenu.Item
              as="button"
              className="inline-flex items-center py-2 px-3 h4 text-black bg-gray-100 rounded default-ring"
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

      {showOverlay && open && (
        <div className="fixed z-30 right-0 bottom-0 w-screen h-screen bg-black opacity-30" />
      )}
    </>
  );
};
