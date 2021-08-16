import { DialogContent } from '@radix-ui/react-dialog';
import * as Portal from '@radix-ui/react-portal';
import classNames from 'classnames';
import React, { FunctionComponent, useCallback, useEffect, useRef, useState } from 'react';
import { Link, Route, Switch, useHistory } from 'react-router-dom';
import create from 'zustand';
import { Dialog } from '../components/Dialog';
import { Help } from './Help';
import { Leap } from './Leap';
import { Notifications } from './Notifications';
import { Search } from './Search';
import { SystemMenu } from './SystemMenu';
import { SystemPreferences } from './SystemPreferences';

export interface MatchItem {
  value: string;
  display?: string;
}

interface LeapStore {
  rawInput: string;
  searchInput: string;
  matches: MatchItem[];
  selection: React.ReactNode;
  select: (selection: React.ReactNode, input?: string) => void;
}

export const useLeapStore = create<LeapStore>((set) => ({
  rawInput: '',
  searchInput: '',
  matches: [],
  selection: null,
  select: (selection: React.ReactNode, input?: string) =>
    set({
      rawInput: input || '',
      searchInput: input || '',
      selection
    })
}));

export type MenuState =
  | 'closed'
  | 'search'
  | 'notifications'
  | 'help-and-support'
  | 'system-preferences';

interface NavProps {
  menu?: MenuState;
}

export const Nav: FunctionComponent<NavProps> = ({ menu = 'closed' }) => {
  const { push } = useHistory();
  const inputRef = useRef<HTMLInputElement>(null);
  const navRef = useRef<HTMLDivElement>(null);
  const dialogNavRef = useRef<HTMLDivElement>(null);
  const { selection, select } = useLeapStore((state) => ({
    selection: state.selection,
    select: state.select
  }));
  const [systemMenuOpen, setSystemMenuOpen] = useState(false);
  const [dialogContentOpen, setDialogContentOpen] = useState(false);

  const isOpen = menu !== 'closed';
  const eitherOpen = isOpen || systemMenuOpen;

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

  useEffect(() => {
    inputRef.current?.focus();
  }, [selection]);

  const onDialogClose = useCallback((open: boolean) => {
    if (!open) {
      select(null);
      setDialogContentOpen(false);
      push('/');
    }
  }, []);

  return (
    <>
      <Portal.Root
        containerRef={dialogContentOpen ? dialogNavRef : navRef}
        className="flex space-x-2"
      >
        <SystemMenu
          open={systemMenuOpen}
          setOpen={setSystemMenuOpen}
          className={classNames('relative z-50 flex-none', eitherOpen ? 'bg-white' : 'bg-gray-100')}
        />
        <Link
          to="/leap/notifications"
          className="relative z-50 flex-none circle-button bg-blue-400 text-white"
        >
          3
        </Link>
        <Leap ref={inputRef} menu={menu} className={!isOpen ? 'bg-gray-100' : ''} />
      </Portal.Root>
      <menu
        ref={navRef}
        className={classNames(
          'w-full max-w-3xl my-6 px-4 text-gray-400 font-semibold',
          dialogContentOpen && 'h-12'
        )}
      />
      <Dialog open={isOpen} onOpenChange={onDialogClose}>
        <DialogContent
          onOpenAutoFocus={onOpen}
          className="fixed top-0 left-[calc(50%-7.5px)] w-[calc(100%-15px)] max-w-3xl px-4 text-gray-400 -translate-x-1/2 outline-none"
        >
          <header ref={dialogNavRef} className="my-6" />
          <div
            id="leap-items"
            className="grid grid-rows-[fit-content(calc(100vh-7.5rem))] bg-white rounded-3xl overflow-hidden"
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
