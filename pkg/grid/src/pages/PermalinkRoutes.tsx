import { Pikes } from '@urbit/api';
import React, { useEffect } from 'react';
import { Switch, Route, Redirect, RouteComponentProps } from 'react-router-dom';
import { Spinner } from '../components/Spinner';
import { useQuery } from '../logic/useQuery';
import { useCharge } from '../state/docket';
import { useKilnLoaded, usePikes } from '../state/kiln';
import { getAppHref } from '../state/util';

function getDeskByForeignRef(pikes: Pikes, ship: string, desk: string): string | undefined {
  const found = Object.entries(pikes).find(
    ([, pike]) => pike.sync?.ship === ship && pike.sync?.desk === desk
  );
  return found ? found[0] : undefined;
}

type AppLinkProps = RouteComponentProps<{
  ship: string;
  desk: string;
  link: string;
}>;

function AppLink({ match, history, location }: AppLinkProps) {
  const { ship, desk, link = '' } = match.params;
  const pikes = usePikes();
  const ourDesk = getDeskByForeignRef(pikes, ship, desk);

  if (ourDesk) {
    return <AppLinkRedirect desk={ourDesk} link={link} />;
  }
  return <AppLinkNotFound match={match} history={history} location={location} />;
}

function AppLinkNotFound({ match }: AppLinkProps) {
  const { ship, desk } = match.params;
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
    if (!charge) {
      return;
    }

    const query = new URLSearchParams({
      'grid-link': encodeURIComponent(`/${link}`)
    });

    const url = `${getAppHref(charge.href)}?${query.toString()}`;
    window.open(url, desk);
  }, [charge]);

  return <Redirect to="/" />;
}

const LANDSCAPE_DESK = 'landscape';
const LANDSCAPE_HOST = '~lander-dister-dozzod-dozzod';

function LandscapeLink({ match }: RouteComponentProps<{ link: string }>) {
  const { link } = match.params;

  return <Redirect to={`/perma/${LANDSCAPE_HOST}/${LANDSCAPE_DESK}/group/${link}`} />;
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
