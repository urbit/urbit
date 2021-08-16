import classNames from 'classnames';
import { debounce } from 'lodash-es';
import React, {
  ChangeEvent,
  FocusEvent,
  FormEvent,
  HTMLAttributes,
  useCallback,
  useEffect,
  useImperativeHandle,
  useRef
} from 'react';
import { Link, useHistory, useRouteMatch } from 'react-router-dom';
import { Cross } from '../components/icons/Cross';
import { MenuState, useNavStore } from './Nav';

type LeapProps = {
  menu: MenuState;
} & HTMLAttributes<HTMLDivElement>;

export const Leap = React.forwardRef(({ menu, className }: LeapProps, ref) => {
  const { push } = useHistory();
  const match = useRouteMatch<{ query?: string; desk?: string }>(
    `/leap/${menu}/:query?/(apps)?/:desk?`
  );
  const appsMatch = useRouteMatch(`/leap/${menu}/${match?.params.query}/apps`);
  const inputRef = useRef<HTMLInputElement>(null);
  useImperativeHandle(ref, () => inputRef.current);
  const { searchInput, setSearchInput, selection, select } = useNavStore();

  const navigateByInput = useCallback(
    (input: string) => {
      const normalizedValue = input.trim().replace(/(~?[\w^_-]{3,13})\//, '$1/apps/');
      push(`/leap/${menu}/${normalizedValue}`);
    },
    [menu]
  );

  const handleSearch = useCallback(
    debounce(
      (input: string) => {
        if (!match || appsMatch) {
          return;
        }

        navigateByInput(input);
      },
      300,
      { leading: true }
    ),
    [menu, match]
  );

  useEffect(() => {
    if (searchInput) {
      handleSearch(searchInput);
    }
  }, [searchInput]);

  const toggleSearch = useCallback(() => {
    if (selection || menu === 'search') {
      return;
    }

    push('/leap/search');
  }, [selection, menu]);

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

  const onSubmit = useCallback(
    (e: FormEvent<HTMLFormElement>) => {
      e.preventDefault();

      const input = [searchInput];
      if (appsMatch) {
        input.unshift(match?.params.query || '');
      } else {
        input.push('');
      }

      navigateByInput(input.join('/'));
    },
    [searchInput, match]
  );

  return (
    <form
      className={classNames(
        'relative z-50 flex items-center w-full px-2 rounded-full bg-white default-ring focus-within:ring-4',
        className
      )}
      onSubmit={onSubmit}
    >
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
    </form>
  );
});
