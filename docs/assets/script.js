const splashes = [
  '«What if we added everything?»',
	'Featured on the <a href="https://www.arraycast.com/episodes/episode88-tinyapl">Array Cast</a>!',
	'Featured on <a href="https://www.youtube.com/watch?v=iQ7mnodrclY">Tacit Talk</a>!',
	'Tiny might be a misnomer.',
	'Check out the <a href="https://blog.rubenverg.com">blog</a>!',
	'Now with 100% more stolen features!',
]

document.querySelector('#splash').innerHTML = splashes[Math.floor(Math.random() * splashes.length)];

document.querySelector('#font').addEventListener('input', () => {
	document.body.className = document.querySelector('#font').value;
});