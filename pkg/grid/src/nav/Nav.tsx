import { DialogContent } from '@radix-ui/react-dialog';
import * as Portal from '@radix-ui/react-portal';
import classNames from 'classnames';
import React, { FunctionComponent, useCallback, useEffect, useRef, useState } from 'react';
import { Route, Switch, useHistory, useRouteMatch } from 'react-router-dom';
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
  url: string;
  openInNewTab: boolean;
  value: string;
  display?: string;
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

interface NavProps {
  menu?: MenuState;
}

export const Nav: FunctionComponent<NavProps> = ({ menu }) => {
  const { push } = useHistory();
  const inputRef = useRef<HTMLInputElement>(null);
  const navRef = useRef<HTMLDivElement>(null);
  const dialogNavRef = useRef<HTMLDivElement>(null);
  const systemMenuOpen = useRouteMatch('/system-menu');
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

  const preventClose = useCallback((e) => {
    const target = e.target as HTMLElement;
    const hasNavAncestor = target.closest('#dialog-nav');

    if (hasNavAncestor) {
      e.preventDefault();
    }
  }, []);

  return (
    <>
      {/* Using portal so that we can retain the same nav items both in the dialog and in the base header */}
      <Portal.Root
        containerRef={dialogContentOpen ? dialogNavRef : navRef}
        className="flex justify-center w-full space-x-2"
      >
        <SystemMenu
          open={!!systemMenuOpen}
          subMenuOpen={menu === 'system-preferences' || menu === 'help-and-support'}
          shouldDim={isOpen && menu !== 'system-preferences' && menu !== 'help-and-support'}
          className={classNames('relative z-50 flex-none', eitherOpen ? 'bg-white' : 'bg-gray-50')}
        />
        <NotificationsLink
          navOpen={isOpen}
          notificationsOpen={menu === 'notifications'}
          shouldDim={(isOpen && menu !== 'notifications') || !!systemMenuOpen}
        />
        <Leap
          ref={inputRef}
          menu={menuState}
          dropdown="leap-items"
          navOpen={isOpen}
          shouldDim={(isOpen && menu !== 'search') || !!systemMenuOpen}
        />
      </Portal.Root>
      <div
        ref={navRef}
        className={classNames(
          'w-full max-w-[712px] mx-auto my-6 text-gray-400 font-semibold',
          dialogContentOpen && 'h-12'
        )}
        role="combobox"
        aria-controls="leap-items"
        aria-owns="leap-items"
        aria-expanded={isOpen}
      />
      <Dialog open={isOpen} onOpenChange={onDialogClose}>
        <DialogContent
          onInteractOutside={preventClose}
          onOpenAutoFocus={onOpen}
          className="fixed bottom-0 sm:top-0 sm:bottom-auto scroll-left-50 flex flex-col scroll-full-width max-w-[882px] px-4 sm:pb-4 text-gray-400 -translate-x-1/2 outline-none"
          role="combobox"
          aria-controls="leap-items"
          aria-owns="leap-items"
          aria-expanded={isOpen}
        >
          <header
            id="dialog-nav"
            ref={dialogNavRef}
            className="max-w-[712px] w-full mx-auto my-6 sm:mb-3 order-last sm:order-none"
          />
          <div
            id="leap-items"
            className="grid grid-rows-[fit-content(calc(100vh-6.25rem))] bg-white rounded-3xl overflow-hidden default-ring focus-visible:ring-2"
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
