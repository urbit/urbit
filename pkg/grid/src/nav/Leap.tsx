import classNames from 'classnames';
import { debounce } from 'lodash-es';
import React, {
  ChangeEvent,
  FocusEvent,
  FormEvent,
  KeyboardEvent,
  HTMLAttributes,
  useCallback,
  useImperativeHandle,
  useRef
} from 'react';
import { Link, useHistory, useRouteMatch } from 'react-router-dom';
import slugify from 'slugify';
import { Cross } from '../components/icons/Cross';
import { MenuState, useLeapStore } from './Nav';

function normalizePathEnding(path: string) {
  const end = path.length - 1;
  return path[end] === '/' ? path.substring(0, end - 1) : path;
}

export function createPreviousPath(current: string): string {
  const parts = normalizePathEnding(current).split('/');
  parts.pop();

  if (parts[parts.length - 1] === 'leap') {
    parts.push('search');
  }

  return parts.join('/');
}

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
  const { rawInput, searchInput, matches, selection, select } = useLeapStore();

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

  const getMatch = useCallback(
    (value: string) => {
      return matches.find((m) => m.display?.startsWith(value) || m.value.startsWith(value));
    },
    [matches]
  );

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

        useLeapStore.setState({ searchInput: input });
        navigateByInput(input);
      },
      300,
      { leading: true }
    ),
    [menu, match]
  );

  const onChange = useCallback(
    (e: ChangeEvent<HTMLInputElement>) => {
      const input = e.target as HTMLInputElement;
      const value = input.value.trim();
      const isDeletion = (e.nativeEvent as InputEvent).inputType === 'deleteContentBackward';
      const inputMatch = getMatch(value);
      const matchValue = inputMatch?.display || inputMatch?.value;

      if (matchValue && inputRef.current && !isDeletion) {
        inputRef.current.value = matchValue;
        inputRef.current.setSelectionRange(value.length, matchValue.length);
        useLeapStore.setState({ rawInput: matchValue });
      } else {
        useLeapStore.setState({ rawInput: value });
      }

      handleSearch(value);
    },
    [matches]
  );

  const onSubmit = useCallback(
    (e: FormEvent<HTMLFormElement>) => {
      e.preventDefault();

      const value = inputRef.current?.value.trim();

      if (!value) {
        return;
      }

      const input = [slugify(getMatch(value)?.value || value)];
      if (appsMatch) {
        input.unshift(match?.params.query || '');
      } else {
        input.push('');
      }

      navigateByInput(input.join('/'));
      useLeapStore.setState({ rawInput: '' });
    },
    [match]
  );

  const onKeyDown = useCallback(
    (e: KeyboardEvent<HTMLDivElement>) => {
      if ((!selection && rawInput) || rawInput) {
        return;
      }

      if (e.key === 'Backspace' || e.key === 'Delete') {
        e.preventDefault();
        select(null, appsMatch ? undefined : match?.params.query);
        const pathBack = createPreviousPath(match?.url || '');
        push(pathBack);
      }
    },
    [selection, rawInput, match]
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
        value={rawInput}
        onClick={toggleSearch}
        onFocus={onFocus}
        onChange={onChange}
        onKeyDown={onKeyDown}
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
