import { Action, Box, Col, Icon, Row, Text } from '@tlon/indigo-react';
import React, { ReactElement, ReactNode, useEffect } from 'react';
import Helmet from 'react-helmet';
import { Link, Route, Switch, useHistory, useLocation } from 'react-router-dom';
import useHarkState from '~/logic/state/hark';
import { Body } from '~/views/components/Body';
import { StatelessAsyncAction } from '~/views/components/StatelessAsyncAction';
import { Archive } from './Archive';
import { NewBox } from './NewBox';

const baseUrl = '/~notifications';

export function NavLink({
  href,
  children
}: {
  href: string;
  children: ReactNode;
}) {
  const location = useLocation();
  const { push } = useHistory();

  const isActive = href === location.pathname;

  const onClick = () => {
    push(href);
  };

  return (
    <Action
      backgroundColor="transparent"
      onClick={onClick}
      color={isActive ? 'black' : 'gray'}
    >
      {children}
    </Action>
  );
}

export default function NotificationsScreen(props: any): ReactElement {
  const relativePath = (p: string) => baseUrl + p;

  const notificationsCount = useHarkState(state => state.notificationsCount);
  const onReadAll = async () => {};

  useEffect(() => {
    function visibilitychange() {
      if (document.visibilityState === 'hidden') {
        useHarkState.getState().opened();
      }
    }
    document.addEventListener('visibilitychange', visibilitychange);

    return () => {
      document.removeEventListener('visibilitychange', visibilitychange);
      useHarkState.getState().opened();
    };
  }, []);

  return (
    <Switch>
      <Route
        path={[relativePath('/:view'), relativePath('')]}
        render={(routeProps) => {
          const { view } = routeProps.match.params;
          return (
            <>
              <Helmet defer={false}>
                <title>
                  {notificationsCount ? `(${String(notificationsCount)}) ` : ''}
                  Notifications
                </title>
              </Helmet>
              <Body>
                <Col overflowY="hidden" height="100%">
                  <Row
                    p={3}
                    alignItems="center"
                    height="48px"
                    justifyContent="space-between"
                    width="100%"
                    borderBottom={1}
                    borderBottomColor="lightGray"
                  >
                    <Text
                      fontWeight="bold"
                      fontSize={2}
                      lineHeight={1}
                    >
                      Notifications
                    </Text>
                    <Row gapX="2">
                      <NavLink href="/~notifications">New</NavLink>
                      <NavLink href="/~notifications/archive">Archive</NavLink>
                    </Row>
                    <Row justifyContent="space-between" gapX={3}>
                      { (false as boolean) ? (
                        <StatelessAsyncAction
                          overflow="hidden"
                          color="black"
                          backgroundColor="white"
                          onClick={onReadAll}
                        >
                          Mark All Read
                        </StatelessAsyncAction>
                      ) : null}
                      <Link to="/~settings#notifications">
                        <Box>
                          <Icon lineHeight={1} icon="Adjust" />
                        </Box>
                      </Link>
                    </Row>
                  </Row>
                  { view === 'archive' ? (
                    <Archive />
                  ) : <NewBox /> }
                </Col>
              </Body>
            </>
          );
        }}
      />
    </Switch>
  );
}
