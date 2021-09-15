import React, { useEffect } from 'react';
import { Switch, Route, Redirect, RouteComponentProps } from 'react-router-dom';
import { Spinner } from '../components/Spinner';
import { useQuery } from '../logic/useQuery';
import { useCharge } from '../state/docket';
import useKilnState, { useKilnLoaded } from '../state/kiln';
import { getAppHref } from '../state/util';

function getDeskByForeignRef(ship: string, desk: string): string | undefined {
  const { vats } = useKilnState.getState();
  console.log(ship, desk);
  const found = Object.entries(vats).find(
    ([, vat]) => vat.arak.ship === ship && vat.arak.desk === desk
  );
  return found ? found[0] : undefined;
}

type AppLinkProps = RouteComponentProps<{
  ship: string;
  desk: string;
  link: string;
}>;

/* eslint-disable react/destructuring-assignment */
function AppLink(props: AppLinkProps) {
  const { ship, desk, link = '' } = props.match.params;
  const ourDesk = getDeskByForeignRef(ship, desk);

  if (ourDesk) {
    return <AppLinkRedirect desk={ourDesk} link={link} />;
  }
  return <AppLinkNotFound {...props} />;
}

function AppLinkNotFound(props: AppLinkProps) {
  const { ship, desk } = props.match.params;
  return <Redirect to={`/leap/search/direct/apps/${ship}/${desk}`} />;
}

function AppLinkInvalid() {
  return (
    <div>
      <h4>Link was malformed</h4>
      <p>The link you tried to follow was invalid</p>
    </div>
  );
}
function AppLinkRedirect({ desk, link }: { desk: string; link: string }) {
  const charge = useCharge(desk);
  useEffect(() => {
    const query = new URLSearchParams({
      'grid-link': encodeURIComponent(`/${link}`)
    });
    const url = `${getAppHref(charge.href)}?${query.toString()}`;
    window.open(url, desk);
  }, []);
  return <Redirect to="/" />;
}

const LANDSCAPE_SHIP = '~zod';
const LANDSCAPE_DESK = 'groups';

function LandscapeLink(props: RouteComponentProps<{ link: string }>) {
  const { link } = props.match.params;

  return <Redirect to={`/perma/${LANDSCAPE_SHIP}/${LANDSCAPE_DESK}/${link}`} />;
}

export function PermalinkRoutes() {
  const loaded = useKilnLoaded();

  const { query } = useQuery();

  if (query.has('ext')) {
    const ext = query.get('ext')!;
    const url = `/perma${ext.slice(16)}`;
    return <Redirect to={url} />;
  }

  if (!loaded) {
    return <Spinner />;
  }

  return (
    <Switch>
      <Route path="/perma/group/:link+" component={LandscapeLink} />
      <Route path="/perma/:ship/:desk/:link*" component={AppLink} />
      <Route path="/" component={AppLinkInvalid} />
    </Switch>
  );
}
