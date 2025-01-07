/** @jsx h */
/** @jsxFrag Fragment */

import { Pages } from '../types.d.ts';

import { h, Fragment } from '../deps/x/htm.ts';

export interface HeaderProps {
	pages: Pages;
}

function Header({ pages }: HeaderProps) {
	const sidebarId = 'sidebar';
	const sidebarNavClass = 'nav flex-column mb-0 mb-sm-auto align-items-start', sidebarSubNavClass = `${sidebarNavClass} ms-1`;

	const sidebar = <div id={sidebarId} class='offcanvas offcanvas-start w-auto' tabindex={-1} data-bs-keyboard='false' data-bs-backdrop='false'>
		<div class='offcanvas-header'>
			<h6 class='offcanvas-title d-none d-sm-block'>Docs</h6>
			<button type='button' class='btn-close text-reset' data-bs-dismiss='offcanvas' aria-label='Close' />
		</div>
		<div class='offcanvas-body'>
			<ul class={sidebarNavClass}>
				<li class='nav-item'>
					<a class='nav-link' href='/'>Index</a>
					<a class='nav-link' href='/run'>Run Interpreter</a>
				</li>
				<li class='nav-item ms-2'>
					Info
					<ul class={sidebarSubNavClass}>
						{[...Object.entries(pages.info)].sort(([_ak, av], [_bk, bv]) => av.name.localeCompare(bv.name)).map(([id, info]) => <li class='nav-item'><a class='nav-link' href={`/docs/info/${id}`}>{info.name}</a></li>)}
					</ul>
				</li>
				<li class='nav-item ms-2'>
					<a class='nav-link ps-0' href='/docs/primitive'>Primitives</a>
					<ul class={sidebarSubNavClass}>
						{[...Object.entries(pages.primitives)].sort(([_ak, av], [_bk, bv]) => av.name.localeCompare(bv.name)).map(([id, primitive]) => <li class='nav-item'><a class='nav-link' href={`/docs/primitive/${id}`}><code>{primitive.glyph}</code> {primitive.name}</a></li>)}
					</ul>
				</li>
				<li class='nav-item ms-2'>
					Quad Names
					<ul class={sidebarSubNavClass}>
						{[...Object.entries(pages.quads)].sort(([_ak, av], [_bk, bv]) => av.glyph.localeCompare(bv.glyph)).map(([id, quad]) => <li class='nav-item'><a class='nav-link' href={`/docs/quad/${id}`}><code>{quad.glyph}</code> {quad.name}</a></li>)}
					</ul>
				</li>
				<li class='nav-item ms-2'>
					<a class='nav-link ps-0' href='/docs/glyph'>Glyphs</a>
					<ul class={sidebarSubNavClass}>
						{[...Object.entries(pages.glyphs)].sort(([_ak, av], [_bk, bv]) => av.name.localeCompare(bv.name)).map(([id, glyph]) => <li class='nav-item'><a class='nav-link' href={`/docs/glyph/${id}`}><code>{glyph.glyph}</code> {glyph.name}</a></li>)}
					</ul>
				</li>
			</ul>
		</div>
	</div>;

	return <>
		{sidebar}
		<nav class='navbar navbar-expand-md bg-body-tertiary'>
			<div class='container-fluid'>
			<button type='button' class='btn' data-bs-toggle='offcanvas' data-bs-target={'#' + sidebarId}><i class='bi bi-list' /></button>
				<a class='navbar-brand' href='/'>
					<img src='/logo.svg' alt='TinyAPL' height='24' class='d-inline-block align-text-top' />&#x2009;
					TinyAPL
				</a>
				<em id='splash' class='text-body-secondary' />
				<div class='collapse navbar-collapse'>
					<ul class='navbar-nav ms-auto'>
						<li class='nav-item d-inline-flex align-items-baseline'>
							<a class='nav-link pe-1' href='/run/latest'>Run</a>
							<small><a class='nav-link p-0' href='/run'>(choose version)</a></small>
						</li>
						<li class='nav-item'>
							<a class='nav-link' href='/docs/primitive'>Primitives</a>
						</li>
					</ul>
				</div>
			</div>
		</nav>
	</>;
}

export default Header;