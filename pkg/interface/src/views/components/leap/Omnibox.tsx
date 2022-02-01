import { Box, Row, Text } from '@tlon/indigo-react';
import { omit } from 'lodash';
import Mousetrap from 'mousetrap';
import fuzzy from 'fuzzy';
import _ from 'lodash';
import React, {
  ReactElement, useCallback,
  useEffect, useMemo,
  useRef,

  useState
} from 'react';
import { useHistory, useLocation } from 'react-router-dom';
import * as ob from 'urbit-ob';
import defaultApps from '~/logic/lib/default-apps';
import makeIndex, { OmniboxItem } from '~/logic/lib/omnibox';
import { useOutsideClick } from '~/logic/lib/useOutsideClick';
import { deSig } from '~/logic/lib/util';
import useContactState from '~/logic/state/contact';
import useGroupState from '~/logic/state/group';
import useHarkState from '~/logic/state/hark';
import useInviteState from '~/logic/state/invite';
import { withLocalState } from '~/logic/state/local';
import useMetadataState from '~/logic/state/metadata';
import useSettingsState, { SettingsState } from '~/logic/state/settings';
import { Portal } from '../Portal';
import OmniboxInput from './OmniboxInput';
import OmniboxResult from './OmniboxResult';

interface OmniboxProps {
  show: boolean;
  toggle: () => void;
  notifications: number;
}

const SEARCHED_CATEGORIES = [
  'commands',
  'ships',
  'other',
  'groups',
  'subscriptions',
];
const settingsSel = (s: SettingsState) => s.leap;
const CAT_LIMIT = 6;

/**
 * Flatten `catMap` according to ordering in `cats`
 */
function flattenCattegoryMap(cats: string[], catMap: Map<string, OmniboxItem[]>) {
  let res = [] as OmniboxItem[];
  cats.forEach((cat) => {
    res = res.concat(_.take(catMap.get(cat), CAT_LIMIT));
  });

  return res;
}

export function Omnibox(props: OmniboxProps): ReactElement {
  const location = useLocation();
  const history = useHistory();
  const leapConfig = useSettingsState(settingsSel);
  const omniboxRef = useRef<HTMLDivElement | null>(null);
  const inputRef = useRef<HTMLInputElement | null>(null);

  const [query, setQuery] = useState('');
  const [selected, setSelected] = useState<[] | [string, string]>([]);
  const contactState = useContactState(state => state.contacts);
  const notificationCount = useHarkState(state => state.notificationsCount);
  const invites = useInviteState(state => state.invites);
  const [leapCursor, setLeapCursor] = useState('pointer');

  const contacts = useMemo(() => {
    const maybeShip = `~${deSig(query)}`;
    const selflessContactState = omit(contactState, `~${window.ship}`);
    return ob.isValidPatp(maybeShip) && maybeShip !== `~${window.ship}`
      ? { ...selflessContactState, [maybeShip]: {} }
      : selflessContactState;
  }, [contactState, query]);

  const groups = useGroupState(state => state.groups);
  const associations = useMetadataState(state => state.associations);

  const selectedGroup = useMemo(
    () =>
      location.pathname.startsWith('/~landscape/ship/')
        ? '/' + location.pathname.split('/').slice(2, 5).join('/')
        : null,
    [location.pathname]
  );

  const index = useMemo(() => {
    return makeIndex(
      contacts,
      associations,
      selectedGroup,
      groups,
      leapConfig
    );
  }, [selectedGroup, leapConfig, contacts, associations, groups]);

  const onOutsideClick = useCallback(() => {
    props.show && props.toggle();
  }, [props.show, props.toggle]);

  useOutsideClick(omniboxRef, onOutsideClick);

  //  handle omnibox show
  useEffect(() => {
    if (!props.show) {
      return;
    }
    Mousetrap.bind('escape', props.toggle);
    const touchstart = new Event('touchstart');
    // @ts-ignore ref typings
    inputRef?.current?.input?.dispatchEvent(touchstart);
    // @ts-ignore ref typings
    inputRef?.current?.input?.focus();
    return () => {
      Mousetrap.unbind('escape');
      setQuery('');
    };
  }, [props.show]);

  const initialResults = useMemo(() => {
    return new Map<string, OmniboxItem[]>(
      SEARCHED_CATEGORIES.map((category) => {
        if (category === 'other') {
          return [
            'other',
            index.get('other')
          ];
        }
        return [category, []];
      })
    );
  }, [index]);

  const [results, categoryOrder] = useMemo(
    (): [Map<string, OmniboxItem[]>, string[]]  => {
    if (query.length <= 1) {
      return [initialResults, ['other']];
    }
    const q = query.toLowerCase();
    const resultsMap = new Map<string, OmniboxItem[]>();
    const categoryMaxes: Record<string, number> = {};

    SEARCHED_CATEGORIES.map((category) => {
      const categoryIndex = index.get(category);
      const fuzzied = fuzzy
        .filter(q, categoryIndex, { extract: res => res.title });
      categoryMaxes[category] = fuzzied
        .map(a => a.score)
        .reduce((a,b) => Math.max(a,b), 0);
      resultsMap.set(category, fuzzied.map(a => a.original));
    });
    const order = Object.entries(categoryMaxes)
      .sort(([,a],[,b]) => b - a)
      .map(([id]) => id);
    return [resultsMap, order];
  }, [query, index]);

  const navigate = useCallback(
    (app: string, link: string, shift: boolean) => {
      props.toggle();
      if (
        defaultApps.includes(app.toLowerCase()) ||
        app === 'profile' ||
        app === 'messages' ||
        app === 'Links' ||
        app === 'Terminal' ||
        app === 'home' ||
        app === 'notifications'
      ) {
        if(shift && app === 'profile') {
          // TODO: hacky, fix
          link = link.replace('~profile', '~landscape/messages/dm');
        }
        if(link.startsWith('?')) {
          history.push({
            search: link
          });
        } else {
          history.push(link);
        }
      } else {
        window.location.href = link;
      }
    },
    [history, props.toggle]
  );

  const setPreviousSelected = useCallback(() => {
    const flattenedResults = flattenCattegoryMap(categoryOrder, results);
    const totalLength = flattenedResults.length;
    if (selected.length) {
      const currentIndex = flattenedResults.indexOf(
          // @ts-ignore unclear how to give this spread a return signature
        ...flattenedResults.filter((e) => {
          return e.link === selected[1];
        })
      );
      if (currentIndex > 0) {
        const { app, link } = flattenedResults[currentIndex - 1];
        setSelected([app, link]);
      } else {
        const { app, link } = flattenedResults[totalLength - 1];
        setSelected([app, link]);
      }
    } else {
      const { app, link } = flattenedResults[totalLength - 1];
      setSelected([app, link]);
    }
  }, [results, categoryOrder, selected]);

  const setNextSelected = useCallback(() => {
    const flattenedResults = flattenCattegoryMap(categoryOrder, results);
    if (selected.length) {
      const currentIndex = flattenedResults.indexOf(
        // @ts-ignore unclear how to give this spread a return signature
        ...flattenedResults.filter((e) => {
          return e.link === selected[1];
        })
      );
      if (currentIndex < flattenedResults.length - 1) {
        const { app, link } = flattenedResults[currentIndex + 1];
        setSelected([app, link]);
      } else {
        const { app, link } = flattenedResults[0];
        setSelected([app, link]);
      }
    } else {
      const { app, link } = flattenedResults[0];
      setSelected([app, link]);
    }
  }, [results, categoryOrder, selected]);

  const setSelection = (app, link) => {
    setLeapCursor('pointer');
    setSelected([app, link]);
  };

  const control = useCallback(
    (evt) => {
      if (evt.key === 'Escape') {
        if (query.length > 0) {
          setQuery('');
          return;
        } else if (props.show) {
          props.toggle();
          return;
        }
      }
      if (evt.key === 'ArrowUp' || (evt.shiftKey && evt.key === 'Tab')) {
        evt.preventDefault();
        setPreviousSelected();
        setLeapCursor('none');
        return;
      }
      if (evt.key === 'ArrowDown' || evt.key === 'Tab') {
        evt.preventDefault();
        setNextSelected();
        setLeapCursor('none');
        return;
      }
      if (evt.key === 'Enter') {
        evt.preventDefault();
        let values = flattenCattegoryMap(categoryOrder, results);
        if (selected.length) {
          navigate(selected[0], selected[1], evt.shiftKey);
        } else if (values.length === 0) {
          return;
        } else {
          navigate(
            values[0].app,
            values[0].link,
            evt.shiftKey
          );
        }
      }
    },
    [
      props.toggle,
      selected,
      navigate,
      query,
      props.show,
      results,
      categoryOrder,
      setPreviousSelected,
      setNextSelected
    ]
  );

  useEffect(() => {
    const flattenedResultLinks: [string, string][] = 
      flattenCattegoryMap(categoryOrder, results)
        .map(result => [result.app, result.link]);
    if (!flattenedResultLinks.includes(selected as [string, string])) {
      setSelected(flattenedResultLinks[0] || []);
    }
  }, [results]);

  const search = useCallback((event) => {
    setQuery(event.target.value);
  }, []);

  // Sort Omnibox results alphabetically
  const sortResults = (
    a: Record<'title', string>,
    b: Record<'title', string>
  ) => {
    // Do not sort unless searching (preserves order of menu actions)
    if (query === '') {
      return 0;
    }
    if (a.title < b.title) {
      return -1;
    }
    if (a.title > b.title) {
      return 1;
    }
    return 0;
  };

  const renderResults = useCallback(() => {
    return (
      <Box
        maxHeight={['200px', '400px']}
        overflow='hidden'
        borderBottomLeftRadius={2}
        borderBottomRightRadius={2}
      >
        {categoryOrder.map(category =>
          ({
            category,
            categoryResults: _.take(results.get(category), CAT_LIMIT)
          })
        )
          .filter(category => category.categoryResults.length > 0)
          .map(({ category, categoryResults }, i) => {
            const categoryTitle =
              category === 'other' ? null : (
                <Row pl={2} height={5} alignItems='center' bg='washedGray'>
                  <Text gray bold>
                    {category.charAt(0).toUpperCase() + category.slice(1)}
                  </Text>
                </Row>
              );
            const sel = selected?.length ? selected[1] : '';
            return (
              <Box key={i} width='max(50vw, 300px)' maxWidth='700px'>
                {categoryTitle}
                {categoryResults.map((result, i2) => (
                  <OmniboxResult
                    key={i2}
                    // @ts-ignore withHovering doesn't pass props
                    icon={result.app}
                    text={result.title}
                    subtext={result.host}
                    shiftLink={result.shiftLink}
                    shiftDescription={result.shiftDescription}
                    description={result.description}
                    link={result.link}
                    cursor={leapCursor}
                    navigate={() => navigate(result.app, result.link, false)}
                    setSelection={() => setSelection(result.app, result.link)}
                    selected={sel}
                    hasNotifications={notificationCount !== 0}
                  />
                ))}
              </Box>
            );
          })}
      </Box>
    );
  }, [results, navigate, selected, contactState, invites]);

  return (
    <Portal>
      <Box
        backgroundColor='scales.black30'
        width='100%'
        height='100%'
        position='absolute'
        top={0}
        right={0}
        zIndex={11}
        display={props.show ? 'block' : 'none'}
      >
        <Row justifyContent='center'>
          <Box
            mt={['10vh', '15vh']}
            width='max(50vw, 300px)'
            maxWidth='700px'
            borderRadius={2}
            backgroundColor='white'
            ref={(el) => {
              omniboxRef.current = el;
            }}
          >
            { /* @ts-ignore investigate zustand types */ }
            <OmniboxInput
              ref={(el) => {
                // @ts-ignore investigate refs
                inputRef.current = el;
              }}
              control={e => control(e)}
              search={search}
              query={query}
            />
            {renderResults()}
          </Box>
        </Row>
      </Box>
    </Portal>
  );
}
// @ts-ignore investigate zustand types
export default withLocalState(Omnibox, ['toggleOmnibox', 'omniboxShown']);
