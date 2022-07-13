import { DialogContent } from '@radix-ui/react-dialog';
import * as Portal from '@radix-ui/react-portal';
import classNames from 'classnames';
import React, { FunctionComponent, useCallback, useEffect, useRef, useState } from 'react';
import { ErrorBoundary } from 'react-error-boundary';
import { Link, Route, Switch, useHistory, useRouteMatch } from 'react-router-dom';
import create from 'zustand';
import { Avatar } from '../components/Avatar';
import { Dialog } from '../components/Dialog';
import { ErrorAlert } from '../components/ErrorAlert';
import { Help } from './Help';
import { Leap } from './Leap';
import { Notifications } from './Notifications';
import { NotificationsLink } from './NotificationsLink';
import { Search } from './Search';
import { SystemPreferences } from '../preferences/SystemPreferences';
import { useSystemUpdate } from '../logic/useSystemUpdate';
import { Bullet } from '../components/icons/Bullet';

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
  | 'system-preferences'
  | 'upgrading';

interface NavProps {
  menu?: MenuState;
}

export const Nav: FunctionComponent<NavProps> = ({ menu }) => {
  const { push } = useHistory();
  const inputRef = useRef<HTMLInputElement>(null);
  const navRef = useRef<HTMLDivElement>(null);
  const dialogNavRef = useRef<HTMLDivElement>(null);
  const systemMenuOpen = useRouteMatch('/leap/system-preferences');
  const { systemBlocked } = useSystemUpdate();
  const [dialogContentOpen, setDialogContentOpen] = useState(false);
  const select = useLeapStore((state) => state.select);

  const menuState = menu || 'closed';
  const isOpen = menuState !== 'upgrading' && menuState !== 'closed';

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
    <ErrorBoundary FallbackComponent={ErrorAlert} onReset={() => push('/')}>
      {/* Using portal so that we can retain the same nav items both in the dialog and in the base header */}
      <Portal.Root
        containerRef={dialogContentOpen ? dialogNavRef : navRef}
        className="flex items-center justify-center w-full space-x-2"
      >
        <Link to="/leap/system-preferences" className="relative">
          <Avatar shipName={window.ship} size="nav" />
          {systemBlocked && (
            <Bullet
              className="absolute -top-2 -right-2 h-5 w-5 ml-auto text-orange-500"
              aria-label="System Needs Attention"
            />
          )}
        </Link>
        <NotificationsLink navOpen={isOpen} notificationsOpen={menu === 'notifications'} />
        <Leap
          ref={inputRef}
          menu={menuState}
          dropdown="leap-items"
          navOpen={isOpen}
          systemMenuOpen={!!systemMenuOpen}
        />
      </Portal.Root>
      <div
        ref={navRef}
        className={classNames(
          'w-full max-w-[712px] mx-auto my-6 text-gray-400 font-semibold',
          dialogContentOpen && 'h-9'
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
          className="fixed bottom-0 sm:top-0 sm:bottom-auto scroll-left-50 flex flex-col justify-end sm:justify-start scroll-full-width h-full sm:h-auto max-w-[882px] px-4 sm:pb-4 text-gray-400 -translate-x-1/2 outline-none"
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
            className="grid grid-rows-[fit-content(calc(100vh-6.25rem))] mt-4 sm:mt-0 bg-white rounded-xl overflow-hidden default-ring focus-visible:ring-2"
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
    </ErrorBoundary>
  );
};
