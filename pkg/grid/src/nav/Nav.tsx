import { DialogContent } from '@radix-ui/react-dialog';
import * as Portal from '@radix-ui/react-portal';
import classNames from 'classnames';
import React, { FunctionComponent, useCallback, useEffect, useRef, useState } from 'react';
import { Route, Switch, useHistory } from 'react-router-dom';
import create from 'zustand';
import { Dialog } from '../components/Dialog';
import { Help } from './Help';
import { Leap } from './Leap';
import { Notifications } from './Notifications';
import { NotificationsLink } from './NotificationsLink';
import { Search } from './Search';
import { SystemMenu } from './SystemMenu';
import { SystemPreferences } from './SystemPreferences';

export interface MatchItem {
  value: string;
  display?: string;
  href?: string;
}

interface LeapStore {
  rawInput: string;
  searchInput: string;
  matches: MatchItem[];
  selectedMatch?: MatchItem;
  selection: React.ReactNode;
  select: (selection: React.ReactNode, input?: string) => void;
}

export const useLeapStore = create<LeapStore>((set) => ({
  rawInput: '',
  searchInput: '',
  matches: [],
  selectedMatch: undefined,
  selection: null,
  select: (selection: React.ReactNode, input?: string) =>
    set({
      rawInput: input || '',
      searchInput: input || '',
      selection
    })
}));

window.leap = useLeapStore.getState;

export type MenuState =
  | 'closed'
  | 'search'
  | 'notifications'
  | 'help-and-support'
  | 'system-preferences';

export function createNextPath(current: string, nextPart?: string): string {
  let end = nextPart;
  const parts = current.split('/').reverse();
  if (parts[1] === 'search') {
    end = 'apps';
  }

  if (parts[0] === 'leap') {
    end = `search/${nextPart}`;
  }

  return `${current}/${end}`;
}

export function createPreviousPath(current: string): string {
  const parts = current.split('/');
  parts.pop();

  if (parts[parts.length - 1] === 'leap') {
    parts.push('search');
  }
  if (parts[parts.length - 2] === 'apps') {
    parts.pop();
  }
  return parts.join('/');
}

interface NavProps {
  menu?: MenuState;
}

export const Nav: FunctionComponent<NavProps> = ({ menu }) => {
  const { push } = useHistory();
  const inputRef = useRef<HTMLInputElement>(null);
  const navRef = useRef<HTMLDivElement>(null);
  const dialogNavRef = useRef<HTMLDivElement>(null);
  const [systemMenuOpen, setSystemMenuOpen] = useState(false);
  const [dialogContentOpen, setDialogContentOpen] = useState(false);
  const select = useLeapStore((state) => state.select);

  const menuState = menu || 'closed';
  const isOpen = menuState !== 'closed';
  const eitherOpen = isOpen || systemMenuOpen;

  useEffect(() => {
    if (!isOpen) {
      select(null);
      setDialogContentOpen(false);
    }
  }, [isOpen]);

  const onOpen = useCallback(
    (event: Event) => {
      event.preventDefault();

      setDialogContentOpen(true);
      if (menu === 'search' && inputRef.current) {
        setTimeout(() => {
          inputRef.current?.focus();
        }, 0);
      }
    },
    [menu]
  );

  const onDialogClose = useCallback((open: boolean) => {
    if (!open) {
      push('/');
    }
  }, []);

  const disableCloseWhenDropdownOpen = useCallback(
    (e: Event) => {
      if (systemMenuOpen) {
        e.preventDefault();
      }
    },
    [systemMenuOpen]
  );

  return (
    <>
      {/* Using portal so that we can retain the same nav items both in the dialog and in the base header */}
      <Portal.Root
        containerRef={dialogContentOpen ? dialogNavRef : navRef}
        className="flex justify-center w-full space-x-2"
      >
        <SystemMenu
          open={systemMenuOpen}
          setOpen={setSystemMenuOpen}
          showOverlay={!isOpen}
          className={classNames('relative z-50 flex-none', eitherOpen ? 'bg-white' : 'bg-gray-100')}
        />
        <NotificationsLink isOpen={isOpen} />
        <Leap
          ref={inputRef}
          menu={menuState}
          dropdown="leap-items"
          showClose={isOpen}
          className={classNames('flex-1 max-w-[600px]', !isOpen ? 'bg-gray-100' : '')}
        />
      </Portal.Root>
      <div
        ref={navRef}
        className={classNames(
          'w-full max-w-3xl my-6 px-4 text-gray-400 font-semibold',
          dialogContentOpen && 'h-12'
        )}
        role="combobox"
        aria-controls="leap-items"
        aria-owns="leap-items"
        aria-expanded={isOpen}
      />
      <Dialog open={isOpen} onOpenChange={onDialogClose}>
        <DialogContent
          onOpenAutoFocus={onOpen}
          onInteractOutside={disableCloseWhenDropdownOpen}
          className="fixed bottom-0 sm:top-0 scroll-left-50 flex flex-col scroll-full-width max-w-3xl px-4 pb-4 text-gray-400 -translate-x-1/2 outline-none"
          role="combobox"
          aria-controls="leap-items"
          aria-owns="leap-items"
          aria-expanded={isOpen}
        >
          <header ref={dialogNavRef} className="my-6 order-last sm:order-none" />
          <div
            id="leap-items"
            className="grid grid-rows-[fit-content(100vh)] bg-white rounded-3xl overflow-hidden default-ring"
            tabIndex={0}
            role="listbox"
          >
            <Switch>
              <Route path="/leap/notifications" component={Notifications} />
              <Route path="/leap/system-preferences" component={SystemPreferences} />
              <Route path="/leap/help-and-support" component={Help} />
              <Route path={['/leap/search', '/leap']} component={Search} />
            </Switch>
          </div>
        </DialogContent>
      </Dialog>
    </>
  );
};
