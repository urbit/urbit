import React, { useMemo, useRef, useCallback, useEffect, useState } from 'react';
import { useLocation, useHistory } from 'react-router-dom';
import * as ob from 'urbit-ob';
import Mousetrap from 'mousetrap';

import { Box, Row, Text } from '@tlon/indigo-react';
import { Associations, Contacts, Groups, Invites } from '@urbit/api';

import makeIndex from '~/logic/lib/omnibox';
import OmniboxInput from './OmniboxInput';
import OmniboxResult from './OmniboxResult';
import { deSig } from '~/logic/lib/util';
import { withLocalState } from '~/logic/state/local';

import defaultApps from '~/logic/lib/default-apps';
import {useOutsideClick} from '~/logic/lib/useOutsideClick';
import {Portal} from '../Portal';
import useSettingsState, {SettingsState} from '~/logic/state/settings';
import { Tile } from '~/types';
import useContactState from '~/logic/state/contact';
import useGroupState from '~/logic/state/group';
import useHarkState from '~/logic/state/hark';
import useInviteState from '~/logic/state/invite';
import useLaunchState from '~/logic/state/launch';
import useMetadataState from '~/logic/state/metadata';

interface OmniboxProps {
  show: boolean;
  toggle: () => void;
  notifications: number;
}

const SEARCHED_CATEGORIES = ['ships', 'other', 'commands', 'groups', 'subscriptions', 'apps'];
const settingsSel = (s: SettingsState) => s.leap;

export function Omnibox(props: OmniboxProps) {
  const location = useLocation();
  const history = useHistory();
  const leapConfig = useSettingsState(settingsSel);
  const omniboxRef = useRef<HTMLDivElement | null>(null)
  const inputRef = useRef<HTMLInputElement | null>(null);

  const [query, setQuery] = useState('');
  const [selected, setSelected] = useState<[] | [string, string]>([]);
  const contactState = useContactState(state => state.contacts);
  const notifications = useHarkState(state => state.notifications);
  const invites = useInviteState(state => state.invites);
  const tiles = useLaunchState(state => state.tiles);

  const contacts = useMemo(() => {
    const maybeShip = `~${deSig(query)}`;
    return ob.isValidPatp(maybeShip)
      ? { ...contactState, [maybeShip]: {} }
      : contactState;
  }, [contactState, query]);

  const groups = useGroupState(state => state.groups);
  const associations = useMetadataState(state => state.associations);

  const selectedGroup =  useMemo(
    () => location.pathname.startsWith('/~landscape/ship/')
      ? '/' + location.pathname.split('/').slice(2,5).join('/')
      : null,
    [location.pathname]
  );

  const index = useMemo(() => {
    return makeIndex(
      contacts,
      associations,
      tiles,
      selectedGroup,
      groups,
      leapConfig,
    );
  }, [
    selectedGroup,
    leapConfig,
    contacts,
    associations,
    groups,
    tiles
  ]);

  const onOutsideClick = useCallback(() => {
    props.show && props.toggle();
  }, [props.show, props.toggle]);

  useOutsideClick(omniboxRef, onOutsideClick);

  //  handle omnibox show
  useEffect(() => {
    if(!props.show) {
      return;
    }
    Mousetrap.bind('escape', props.toggle);
    const touchstart = new Event('touchstart');
    inputRef?.current?.input?.dispatchEvent(touchstart);
    inputRef?.current?.input?.focus();
    return () => {
      Mousetrap.unbind('escape');
      setQuery('');
    };
  }, [props.show]);

  const initialResults = useMemo(() => {
    return new Map(SEARCHED_CATEGORIES.map((category) => {
     if (category === 'other') {
       return ['other', index.get('other').filter(({ app }) => app !== 'tutorial')];
     }
     return [category, []];
    }));
  }, [index]);

  const results = useMemo(() => {
    if(query.length <= 1) {
      return initialResults;
    }
    const q = query.toLowerCase();
    const resultsMap = new Map();
    SEARCHED_CATEGORIES.map((category) => {
      const categoryIndex = index.get(category);
      resultsMap.set(category,
        categoryIndex.filter((result) => {
          return (
            result.title.toLowerCase().includes(q) ||
            result.link.toLowerCase().includes(q) ||
            result.app.toLowerCase().includes(q) ||
            (result.host !== null ? result.host.toLowerCase().includes(q) : false)
          );
        })
      );
    });
    return resultsMap;
  }, [query, index]);

  const navigate = useCallback((app: string, link: string) => {
    props.toggle();
    if (defaultApps.includes(app.toLowerCase())
        || app === 'profile'
        || app === 'messages'
        || app === 'tutorial'
        || app === 'Links'
        || app === 'Terminal'
        || app === 'home'
        || app === 'inbox') {
      history.push(link);
    } else {
      window.location.href = link;
    }
  }, [history, props.toggle]);

  const setPreviousSelected = useCallback(() => {
    const flattenedResults = Array.from(results.values()).flat();
    const totalLength = flattenedResults.length;
    if (selected.length) {
      const currentIndex = flattenedResults.indexOf(
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
  }, [results, selected]);

  const setNextSelected = useCallback(() => {
    const flattenedResults = Array.from(results.values()).flat();
    if (selected.length) {
      const currentIndex = flattenedResults.indexOf(
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
  }, [selected, results]);

  const control = useCallback((evt) => {
    if (evt.key === 'Escape') {
      if (query.length > 0) {
        setQuery('');
        return;
      } else if (props.show) {
        props.toggle();
        return;
      }
    }
    if (
      evt.key === 'ArrowUp' ||
      (evt.shiftKey && evt.key === 'Tab')) {
        evt.preventDefault();
      setPreviousSelected();
      return;
    }
    if (evt.key === 'ArrowDown' || evt.key === 'Tab') {
      evt.preventDefault();
      setNextSelected();
      return;
    }
    if (evt.key === 'Enter') {
      evt.preventDefault();
      if (selected.length) {
        navigate(selected[0], selected[1]);
      } else if (Array.from(results.values()).flat().length === 0) {
        return;
      } else {
        navigate(
          Array.from(results.values()).flat()[0].app,
          Array.from(results.values()).flat()[0].link);
      }
    }
  }, [
    props.toggle,
    selected,
    navigate,
    query,
    props.show,
    results,
    setPreviousSelected,
    setNextSelected
  ]);

  useEffect(() => {
    const flattenedResultLinks = Array.from(results.values())
      .flat()
      .map(result => [result.app, result.link]);
    if (!flattenedResultLinks.includes(selected)) {
      setSelected(flattenedResultLinks[0] || []);
    }
  }, [results]);

  const search = useCallback((event) => {
    setQuery(event.target.value);
  }, []);

  const renderResults = useCallback(() => {
    return <Box
            maxHeight={['200px', '400px']}
            overflowY="auto"
            overflowX="hidden"
            borderBottomLeftRadius='2'
            borderBottomRightRadius='2'
           >
      {SEARCHED_CATEGORIES
        .map(category => Object({ category, categoryResults: results.get(category) }))
        .filter(category => category.categoryResults.length > 0)
        .map(({ category, categoryResults }, i) => {
          const categoryTitle = (category === 'other')
            ? null : <Row pl='2' height='5' alignItems='center' bg='washedGray'><Text gray bold>{category.charAt(0).toUpperCase() + category.slice(1)}</Text></Row>;
          const sel = selected?.length ? selected[1] : '';
          return (<Box key={i} width='max(50vw, 300px)' maxWidth='600px'>
            {categoryTitle}
            {categoryResults.map((result, i2) => (
              <OmniboxResult
                key={i2}
                icon={result.app}
                text={result.title}
                subtext={result.host}
                link={result.link}
                navigate={() => navigate(result.app, result.link)}
                selected={sel}
              />
            ))}
          </Box>
        );
      })
      }
    </Box>;
  }, [results, navigate, selected, contactState, notifications, invites]);

  return (
    <Portal>
        <Box
          backgroundColor='scales.black30'
          width='100%'
          height='100%'
          position='absolute'
          top='0'
          right='0'
          zIndex={11}
          display={props.show ? 'block' : 'none'}
        >
          <Row justifyContent='center'>
            <Box
              mt={['10vh', '20vh']}
              width='max(50vw, 300px)'
              maxWidth='600px'
              borderRadius='2'
              backgroundColor='white'
              ref={(el) => {
 omniboxRef.current = el;
}}
            >
              <OmniboxInput
                ref={(el) => {
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

export default withLocalState(Omnibox, ['toggleOmnibox', 'omniboxShown']);
