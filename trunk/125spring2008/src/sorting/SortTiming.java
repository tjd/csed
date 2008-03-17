package sorting;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Random;

import util.Timer;

public class SortTiming {

	public static void main(String[] args) {
		final int N = 2*100000;
		System.out.println("N = " + N);
		int[] unsortedData1 = makeRandomData(N);
		int[] unsortedData2 = copy(unsortedData1);

		Timer timer = new Timer();
		MergeSort.mergeSort(unsortedData1);
		double mergeSortElapsedSeconds = timer.getElapsedSeconds();
		System.out.printf("Mergesort running time: %.2fs\n",
				mergeSortElapsedSeconds);

		timer = new Timer();
		SelectionSort.selectionSort(unsortedData2);
		double selectionSortElapsedSeconds = timer.getElapsedSeconds();
		System.out.printf("\nSelection sort running time: %.2fs\n",
				selectionSortElapsedSeconds);
	}

	public static Random rnd = new Random();

	public static int[] makeRandomData(int n) {
		ArrayList<Integer> arr = new ArrayList<Integer>();
		for (int i = 0; i < n; ++i) {
			arr.add(i);
		}
		Collections.shuffle(arr);
		return copy(arr);
	}

	public static int[] copy(ArrayList<Integer> arr) {
		int[] result = new int[arr.size()];
		for (int i = 0; i < result.length; ++i) {
			result[i] = arr.get(i);
		}
		return result;
	}

	public static int[] copy(int[] arr) {
		int[] result = new int[arr.length];
		for (int i = 0; i < result.length; ++i) {
			result[i] = arr[i];
		}
		return result;
	}
}
