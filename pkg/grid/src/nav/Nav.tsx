import { DialogContent } from '@radix-ui/react-dialog';
import classNames from 'classnames';
import React, {
  ChangeEvent,
  FocusEvent,
  FunctionComponent,
  KeyboardEvent,
  useCallback,
  useEffect,
  useRef,
  useState
} from 'react';
import { Link, Route, Switch, useHistory, useLocation } from 'react-router-dom';
import create from 'zustand';
import { Dialog } from '../components/Dialog';
import { Cross } from '../components/icons/Cross';
import { Help } from './Help';
import { Notifications } from './Notifications';
import { Search } from './Search';
import { SystemMenu } from './SystemMenu';
import { SystemPreferences } from './SystemPreferences';

export type MenuState =
  | 'closed'
  | 'search'
  | 'notifications'
  | 'help-and-support'
  | 'system-preferences';

interface NavProps {
  menu?: MenuState;
}

interface NavStore {
  searchInput: string;
  setSearchInput: (input: string) => void;
  selection: React.ReactNode;
  select: (selection: React.ReactNode, input?: string) => void;
}

export const useNavStore = create<NavStore>((set) => ({
  searchInput: '',
  setSearchInput: (input: string) => set({ searchInput: input }),
  selection: null,
  select: (selection: React.ReactNode, input?: string) =>
    set({ searchInput: input || '', selection })
}));

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

  return parts.join('/');
}

export const Nav: FunctionComponent<NavProps> = ({ menu = 'closed' }) => {
  const { push } = useHistory();
  const location = useLocation();
  const inputRef = useRef<HTMLInputElement>(null);
  const { searchInput, setSearchInput, selection, select } = useNavStore();
  const [systemMenuOpen, setSystemMenuOpen] = useState(false);

  const isOpen = menu !== 'closed';
  const eitherOpen = isOpen || systemMenuOpen;

  const onOpen = useCallback(
    (event: Event) => {
      event.preventDefault();

      if (menu === 'search' && inputRef.current) {
        inputRef.current.focus();
      }
    },
    [menu]
  );

  // useEffect(() => {
  //   if (!menu || menu === 'search') {
  //     select(null);
  //     inputRef.current?.focus();
  //   }
  // }, [menu]);

  useEffect(() => {
    inputRef.current?.focus();
  }, [selection]);

  const toggleSearch = useCallback(() => {
    if (selection || menu === 'search') {
      return;
    }

    push('/leap/search');
  }, [selection, menu]);

  const onDialogClose = useCallback((open: boolean) => {
    if (!open) {
      select(null);
      push('/');
    }
  }, []);

  const onDialogKey = useCallback(
    (e: KeyboardEvent<HTMLDivElement>) => {
      if (!selection || searchInput) {
        return;
      }

      if (e.key === 'Backspace' || e.key === 'Delete') {
        e.preventDefault();
        select(null);
        const pathBack = createPreviousPath(location.pathname);
        push(pathBack);
      }
    },
    [selection, searchInput, location.pathname]
  );

  const onFocus = useCallback((e: FocusEvent<HTMLInputElement>) => {
    // refocusing tab with input focused is false trigger
    const windowFocus = e.nativeEvent.currentTarget === document.body;
    if (windowFocus) {
      return;
    }

    toggleSearch();
  }, []);

  const onChange = useCallback((e: ChangeEvent<HTMLInputElement>) => {
    const input = e.target as HTMLInputElement;
    const value = input.value.trim();
    setSearchInput(value);
  }, []);

  return (
    <menu className="w-full max-w-3xl my-6 px-4 text-gray-400 font-semibold">
      <div className={classNames('flex space-x-2', isOpen && 'invisible')}>
        {!isOpen && (
          <SystemMenu
            showOverlay
            open={systemMenuOpen}
            setOpen={setSystemMenuOpen}
            className={classNames(
              'relative z-50 flex-none',
              eitherOpen ? 'bg-white' : 'bg-gray-100'
            )}
          />
        )}
        <Link
          to="/leap/notifications"
          className="relative z-50 flex-none circle-button bg-blue-400 text-white default-ring"
        >
          3
        </Link>
        <input
          onClick={toggleSearch}
          onFocus={onFocus}
          type="text"
          className="relative z-50 rounded-full w-full pl-4 h4 bg-gray-100 default-ring"
          placeholder="Search Landscape"
        />
      </div>

      <Dialog open={isOpen} onOpenChange={onDialogClose}>
        <DialogContent
          onOpenAutoFocus={onOpen}
          className="fixed top-0 left-[calc(50%-7.5px)] w-[calc(100%-15px)] max-w-3xl px-4 text-gray-400 -translate-x-1/2 outline-none"
        >
          <div tabIndex={-1} onKeyDown={onDialogKey} role="presentation">
            <header className="flex my-6 space-x-2">
              <SystemMenu
                open={systemMenuOpen}
                setOpen={setSystemMenuOpen}
                className={classNames(
                  'relative z-50 flex-none',
                  eitherOpen ? 'bg-white' : 'bg-gray-100'
                )}
              />
              <Link
                to="/leap/notifications"
                className="relative z-50 flex-none circle-button bg-blue-400 text-white"
              >
                3
              </Link>
              <div className="relative z-50 flex items-center w-full px-2 rounded-full bg-white default-ring focus-within:ring-4">
                <label
                  htmlFor="leap"
                  className={classNames(
                    'inline-block flex-none p-2 h4 text-blue-400',
                    !selection && 'sr-only'
                  )}
                >
                  {selection || 'Search Landscape'}
                </label>
                <input
                  id="leap"
                  type="text"
                  ref={inputRef}
                  placeholder={selection ? '' : 'Search Landscape'}
                  className="flex-1 w-full h-full px-2 h4 rounded-full bg-transparent outline-none"
                  value={searchInput}
                  onClick={toggleSearch}
                  onFocus={onFocus}
                  onChange={onChange}
                  role="combobox"
                  aria-controls="leap-items"
                  aria-expanded
                />
                {(selection || searchInput) && (
                  <Link
                    to="/"
                    className="circle-button w-8 h-8 text-gray-400 bg-gray-100 default-ring"
                    onClick={() => select(null)}
                  >
                    <Cross className="w-3 h-3 fill-current" />
                    <span className="sr-only">Close</span>
                  </Link>
                )}
              </div>
            </header>
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
          </div>
        </DialogContent>
      </Dialog>
    </menu>
  );
};
