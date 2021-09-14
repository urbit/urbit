import * as DropdownMenu from '@radix-ui/react-dropdown-menu';
import classNames from 'classnames';
import clipboardCopy from 'clipboard-copy';
import React, { HTMLAttributes, useCallback, useState } from 'react';
import { Link, Route, useHistory } from 'react-router-dom';
import { Vat } from '@urbit/api/hood';
import { Adjust } from '../components/icons/Adjust';
import { useVat } from '../state/kiln';
import { disableDefault, handleDropdownLink } from '../state/util';
import { useMedia } from '../logic/useMedia';

type SystemMenuProps = HTMLAttributes<HTMLButtonElement> & {
  open: boolean;
  shouldDim: boolean;
};

function getHash(vat: Vat): string {
  const parts = vat.hash.split('.');
  return parts[parts.length - 1];
}

export const SystemMenu = ({ className, open, shouldDim }: SystemMenuProps) => {
  const { push } = useHistory();
  const [copied, setCopied] = useState(false);
  const garden = useVat('garden');
  const hash = garden ? getHash(garden) : null;
  const isMobile = useMedia('(max-width: 639px)');

  const copyHash = useCallback((event: Event) => {
    event.preventDefault();

    setCopied(true);
    clipboardCopy('fjuhl');

    setTimeout(() => {
      setCopied(false);
    }, 1250);
  }, []);

  const preventFlash = useCallback((e) => {
    const target = e.target as HTMLElement;

    if (target.id !== 'system-menu-overlay') {
      e.preventDefault();
    }
  }, []);

  return (
    <>
      <div className="z-40">
        <DropdownMenu.Root
          modal={false}
          open={open}
          onOpenChange={(isOpen) => setTimeout(() => !isOpen && push('/'), 15)}
        >
          <DropdownMenu.Trigger
            as={Link}
            to="/system-menu"
            className={classNames(
              'appearance-none circle-button default-ring',
              open && 'text-gray-300',
              shouldDim && 'opacity-60',
              className
            )}
          >
            <Adjust className="w-6 h-6 fill-current text-gray" />
            <span className="sr-only">System Menu</span>
          </DropdownMenu.Trigger>
          <Route path="/system-menu">
            <DropdownMenu.Content
              onCloseAutoFocus={disableDefault}
              onInteractOutside={preventFlash}
              onFocusOutside={preventFlash}
              onPointerDownOutside={preventFlash}
              side={isMobile ? 'top' : 'bottom'}
              sideOffset={12}
              className="dropdown relative z-40 min-w-64 p-4 font-semibold text-gray-500 bg-white"
            >
              <DropdownMenu.Group>
                <DropdownMenu.Item
                  as={Link}
                  to="/leap/system-preferences"
                  className="flex items-center p-2 mb-2 space-x-2 focus:bg-blue-200 focus:outline-none rounded"
                  onSelect={handleDropdownLink()}
                >
                  <span className="w-5 h-5 bg-gray-100 rounded-full" />
                  <span className="h4">System Preferences</span>
                </DropdownMenu.Item>
                <DropdownMenu.Item
                  as={Link}
                  to="/leap/help-and-support"
                  className="flex items-center p-2 mb-2 space-x-2 focus:bg-blue-200 focus:outline-none rounded"
                  onSelect={handleDropdownLink()}
                >
                  <span className="w-5 h-5 bg-gray-100 rounded-full" />
                  <span className="h4">Help and Support</span>
                </DropdownMenu.Item>
                <DropdownMenu.Item
                  as={Link}
                  to="/app/garden"
                  className="flex items-center p-2 mb-2 space-x-2 focus:bg-blue-200 focus:outline-none rounded"
                  onSelect={handleDropdownLink()}
                >
                  <span className="w-5 h-5 bg-gray-100 rounded-full" />
                  <span className="h4">About Landscape</span>
                </DropdownMenu.Item>
                {hash && (
                  <DropdownMenu.Item
                    as="button"
                    className="inline-flex items-center py-2 px-3 m-2 h4 text-black bg-gray-100 rounded focus:bg-blue-200 focus:outline-none"
                    onSelect={copyHash}
                  >
                    <span className="sr-only">Base Hash</span>
                    <code>
                      {!copied && <span aria-label={hash.split('').join('-')}>{hash}</span>}
                      {copied && 'copied!'}
                    </code>
                  </DropdownMenu.Item>
                )}
              </DropdownMenu.Group>
            </DropdownMenu.Content>
          </Route>
        </DropdownMenu.Root>
      </div>
      <Route path="/system-menu">
        <div
          id="system-menu-overlay"
          className="fixed z-30 right-0 bottom-0 w-screen h-screen bg-black opacity-30"
        />
      </Route>
    </>
  );
};
